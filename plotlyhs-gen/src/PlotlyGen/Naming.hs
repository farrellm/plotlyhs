{-# LANGUAGE OverloadedStrings #-}

-- | Haskell names for the schema's enumerations, trace types and modules.
module PlotlyGen.Naming
  ( EnumDef (..),
    Ctor (..),
    nameEnums,
    pascal,
    traceCtor,
    reservedTypes,
    reservedCtors,
  )
where

import Data.Char (isAlphaNum, toUpper)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Scientific (floatingOrInteger)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import PlotlyGen.Schema

-- | A generated enumeration.
data EnumDef = EnumDef
  { enumName :: Text,
    enumKey :: EnumKey,
    enumCtors :: [Ctor],
    -- | The attributes using it, as dotted paths.
    enumUsers :: [Text]
  }

data Ctor
  = -- | A constant constructor and the JSON value it stands for.
    CConst Text EnumVal
  | -- | A constructor carrying a value of the named type.
    CPayload Text Text

-- | Upper-case the first letter of each word.
pascal :: Text -> Text
pascal = T.concat . map cap . T.split (not . isAlphaNum)
  where
    cap w = case T.uncons w of
      Just (c, rest) -> T.cons (toUpper c) rest
      Nothing -> ""

-- | The @TraceType@ constructor for a trace type.
traceCtor :: Text -> Text
traceCtor = pascal

-- | Type names the enumerations must not take: the core's types, the
-- Prelude's, and the trace types (so that @Trace Scatter@ is unambiguous).
reservedTypes :: [Text] -> [Text]
reservedTypes traces =
  map traceCtor traces
    <> [ "Angle",
         "Animation",
         "ArrayOk",
         "Bool",
         "Color",
         "Colorscale",
         "Config",
         "DataArray",
         "Double",
         "Either",
         "Figure",
         "Flags",
         "Frame",
         "Int",
         "IsObject",
         "KnownTraceType",
         "Layout",
         "Maybe",
         "Obj",
         "Ordering",
         "SomeTrace",
         "SubplotId",
         "Text",
         "Trace",
         "TraceType",
         "Type",
         "Value"
       ]

-- | Constructor names the enumerations must not take.
reservedCtors :: [Text] -> [Text]
reservedCtors traces =
  map traceCtor traces
    <> [ "Angle",
         "ColorNumber",
         "ColorString",
         "ColorscaleNamed",
         "ColorscaleStops",
         "DataArray",
         "Figure",
         "Flags",
         "Many",
         "Obj",
         "One",
         "SomeTrace",
         "SubplotId",
         "Trace",
         "False",
         "True",
         "Nothing",
         "Just"
       ]

-- | Name every enumeration. Each starts with the name of the attribute that
-- uses it most (@Xanchor@). Where two would share a name, the one with more
-- users keeps it and the others prepend their parent's name (@XaxisType@),
-- and so on up the path until the names differ.
nameEnums :: [Text] -> [([Text], HsType)] -> Either String [EnumDef]
nameEnums traces uses = do
  let users :: Map EnumKey ([EnumVal], [[Text]])
      users =
        Map.fromListWith
          (\(_, new) (vs, old) -> (vs, old <> new))
          [(k, (vs, [path])) | (path, t) <- uses, (k, vs) <- enumsIn t]
      reps = Map.map (representative . snd) users
      counts = Map.map (length . snd) users
  levels <- settle reps counts (Map.map (const 1) users)
  let defs =
        [ EnumDef
            { enumName = name,
              enumKey = k,
              enumCtors = ctors name k vs,
              enumUsers = map (T.intercalate ".") paths
            }
        | (k, (vs, paths)) <- Map.toList users,
          let name = candidate (reps Map.! k) (levels Map.! k)
        ]
      allCtors = [c | d <- defs, c <- map ctorName (enumCtors d)] <> reservedCtors traces
  case duplicates allCtors of
    [] -> pure (sortOn enumName defs)
    dups -> Left ("duplicate constructor names: " <> show dups)
  where
    reserved = reservedTypes traces
    settle reps counts levels
      | Map.null clashes = pure levels
      | otherwise = do
          let escalate = Map.keysSet (Map.filter id clashes)
              levels' = Map.mapWithKey (\k l -> if k `Set.member` escalate then l + 1 else l) levels
          case [k | k <- Set.toList escalate, levels' Map.! k > length (reps Map.! k)] of
            [] -> settle reps counts levels'
            k : _ -> Left ("cannot name the enumeration used at " <> T.unpack (T.intercalate "." (reps Map.! k)))
      where
        names = Map.mapWithKey (\k l -> candidate (reps Map.! k) l) levels
        groups = Map.fromListWith (<>) [(n, [k]) | (k, n) <- Map.toList names]
        -- True: must escalate. A name shared by several keeps its most-used
        -- holder, if there is exactly one.
        clashes =
          Map.fromList
            [ (k, escalates)
            | (n, ks) <- Map.toList groups,
              n `elem` reserved || length ks > 1,
              let ranked = sortOn (Down . (counts Map.!)) ks
                  winner = case ranked of
                    a : b : _ | counts Map.! a > counts Map.! b, n `notElem` reserved -> Just a
                    [a] | n `notElem` reserved -> Just a
                    _ -> Nothing,
              k <- ks,
              let escalates = Just k /= winner,
              escalates
            ]

-- | The path an enumeration is named after: its most common attribute name,
-- then the shortest and first path with it.
representative :: [[Text]] -> [Text]
representative paths = case sortOn (\p -> (length p, p)) [p | p <- paths, lastOf p == attr] of
  p : _ -> p
  [] -> []
  where
    lastOf = take 1 . reverse
    attrCounts = Map.fromListWith (+) [(lastOf p, 1 :: Int) | p <- paths]
    attr = case sortOn (\(a, n) -> (Down n, a)) (Map.toList attrCounts) of
      (a, _) : _ -> a
      [] -> []

candidate :: [Text] -> Int -> Text
candidate path level = T.concat (map pascal (reverse (take level (reverse path))))

ctorName :: Ctor -> Text
ctorName (CConst n _) = n
ctorName (CPayload n _) = n

ctors :: Text -> EnumKey -> [EnumVal] -> [Ctor]
ctors name k vs =
  dedupe [CConst (name <> suffix v) v | v <- vs]
    <> case ekPayload k of
      Nothing -> []
      Just PSubplot -> [CPayload (name <> "Subplot") "SubplotId"]
      Just PInt -> [CPayload (name <> "Int") "Int"]
      Just PNumber -> [CPayload (name <> "Number") "Double"]
  where
    suffix = \case
      VBool True -> "True"
      VBool False -> "False"
      VNum n -> case floatingOrInteger n :: Either Double Integer of
        Right i | i < 0 -> "Neg" <> T.pack (show (negate i))
        Right i -> T.pack (show i)
        Left d -> T.map (\c -> if c == '.' then '_' else c) (T.pack (show d))
      -- An axis type of "-" means plotly.js picks the type.
      VStr "-" | VStr "linear" `elem` vs -> "Auto"
      VStr ""
        | VStr "none" `elem` vs -> "Empty"
        | otherwise -> "None"
      VStr s
        | T.all (not . isAlphaNum) s -> T.concat (map symbolName (T.unpack s))
        | otherwise -> pascal s
    -- "e" and "E" both give "E"; the later one gets a prime.
    dedupe = go []
      where
        go _ [] = []
        go seen (CConst n v : rest)
          | n `elem` seen = go seen (CConst (n <> "'") v : rest)
          | otherwise = CConst n v : go (n : seen) rest
        go seen (c : rest) = c : go seen rest

symbolName :: Char -> Text
symbolName c = fromMaybe (T.pack ("U" <> show (fromEnum c))) (lookup c table)
  where
    table =
      [ ('/', "Slash"),
        ('\\', "Backslash"),
        ('-', "Dash"),
        ('|', "Pipe"),
        ('+', "Plus"),
        ('.', "Dot"),
        ('=', "Eq"),
        ('<', "Lt"),
        ('>', "Gt"),
        ('[', "Lb"),
        (']', "Rb"),
        ('(', "Lp"),
        (')', "Rp")
      ]

duplicates :: [Text] -> [Text]
duplicates xs = [x | (x, n) <- Map.toList (Map.fromListWith (+) [(x, 1 :: Int) | x <- xs]), n > 1]
