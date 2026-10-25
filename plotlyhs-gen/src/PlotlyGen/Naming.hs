{-# LANGUAGE OverloadedStrings #-}

-- | Haskell names for the schema's enumerations, trace types and modules.
module PlotlyGen.Naming
  ( EnumDef (..),
    Ctor (..),
    Extras (..),
    enumExtras,
    flagCtors,
    extraCtors,
    enumType,
    nameEnums,
    placeholderEnums,
    pascal,
    traceCtor,
    namedKindCtor,
    duplicates,
  )
where

import Data.Char (isAlphaNum, toUpper)
import Data.Either (fromRight)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Ord (Down (..))
import Data.Scientific (floatingOrInteger)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showFFloat)
import PlotlyGen.Schema

-- | A generated enumeration.
data EnumDef = EnumDef
  { enumName :: Text,
    enumKey :: EnumKey,
    enumCtors :: [Ctor],
    -- | The attributes using it, as dotted paths.
    enumUsers :: [Text]
  }

-- | What a flag list may hold instead of its flags.
data Extras
  = -- | Nothing: 'Data.Void.Void'.
    NoExtras
  | -- | @true@ or @false@: 'Bool'.
    BoolExtras
  | -- | Strings: an enumeration of their own, named here.
    NamedExtras Text

-- | A flag list's extras, if it is one.
enumExtras :: EnumDef -> Maybe Extras
enumExtras e
  | not (ekFlags k) = Nothing
  | Set.null (ekExtras k) = Just NoExtras
  | ekExtras k == boolExtras = Just BoolExtras
  | otherwise = Just (NamedExtras (enumName e <> "Extra"))
  where
    k = enumKey e

-- | The constructors of a flag list's flags, or of an enumeration.
flagCtors :: EnumDef -> [Ctor]
flagCtors e = [c | c <- enumCtors e, not (isExtra (enumKey e) c)]

-- | The constructors of a flag list's extras.
extraCtors :: EnumDef -> [Ctor]
extraCtors e = [c | c <- enumCtors e, isExtra (enumKey e) c]

isExtra :: EnumKey -> Ctor -> Bool
isExtra k (CConst _ v) = v `Set.member` ekExtras k
isExtra _ (CPayload _ _) = False

-- | The type of an attribute holding this enumeration: its name, or for a
-- flag list, @Flags Name Extras@.
enumType :: EnumDef -> Text
enumType e = case enumExtras e of
  Nothing -> enumName e
  Just x -> "Flags " <> enumName e <> " " <> extrasType x
  where
    extrasType = \case
      NoExtras -> "Void"
      BoolExtras -> "Bool"
      NamedExtras n -> n

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

-- | The @NamedKind@ constructor for a named subplot kind.
namedKindCtor :: NamedKindDef -> Text
namedKindCtor = ("Named" <>) . pascal . nkKey

-- | Name every enumeration. Each starts with the name of the attribute that
-- uses it most (@Xanchor@). Where two would share a name, the one with more
-- users keeps it and the others prepend their parent's name (@XaxisType@),
-- and so on up the path until the names differ. A name that is reserved
-- (see "PlotlyGen.Reserved"), as a type or a constructor, is not taken
-- either; the notes say which enumerations that renamed, and why.
nameEnums :: Map Text Text -> [([Text], HsType)] -> Either String ([EnumDef], [Text])
nameEnums reservedBy uses = do
  names <- settle (Map.keysSet reservedBy) (Map.map (const 1) users)
  let -- The names they would have if nothing were reserved.
      free = fromRight Map.empty (settle Set.empty (Map.map (const 1) users))
      defs =
        [ EnumDef
            { enumName = name,
              enumKey = k,
              enumCtors = ctors name k vs,
              enumUsers = map (T.intercalate ".") paths
            }
        | (k, (vs, paths)) <- Map.toList users,
          let name = names Map.! k
        ]
      allCtors = [c | d <- defs, c <- map ctorName (enumCtors d)] <> reserved
      allTypes = [n | d <- defs, n <- enumName d : [x | Just (NamedExtras x) <- [enumExtras d]]] <> reserved
      notes =
        [ "the enumeration of "
            <> T.intercalate "." (reps Map.! enumKey d)
            <> " is named "
            <> enumName d
            <> ", not "
            <> n
            <> maybe ", which an enumeration renamed for a reserved name takes" (", which is taken by " <>) (Map.lookup n reservedBy)
        | d <- defs,
          Just n <- [Map.lookup (enumKey d) free],
          n /= enumName d
        ]
  case (duplicates allCtors, duplicates allTypes) of
    ([], []) -> pure (sortOn enumName defs, notes)
    (dups, []) -> Left ("duplicate constructor names: " <> show dups)
    (_, dups) -> Left ("duplicate type names: " <> show dups)
  where
    users = enumUsersOf uses
    reps = Map.map (representative . snd) users
    counts = Map.map (length . snd) users
    reserved = Map.keys reservedBy
    -- The enumerations' names, escalating clashing ones a level at a time.
    settle taken levels
      | Map.null clashes = pure names
      | otherwise = do
          let escalate = Map.keysSet (Map.filter id clashes)
              levels' = Map.mapWithKey (\k l -> if k `Set.member` escalate then l + 1 else l) levels
          case [k | k <- Set.toList escalate, levels' Map.! k > length (reps Map.! k)] of
            [] -> settle taken levels'
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
              let isTaken = n `Set.member` taken,
              isTaken || length ks > 1,
              let ranked = sortOn (Down . (counts Map.!)) ks
                  winner = case ranked of
                    a : b : _ | counts Map.! a > counts Map.! b, not isTaken -> Just a
                    [a] | not isTaken -> Just a
                    _ -> Nothing,
              k <- ks,
              let escalates = Just k /= winner,
              escalates
            ]

-- | Each enumeration's values and the paths of the attributes using it.
enumUsersOf :: [([Text], HsType)] -> Map EnumKey ([EnumVal], [[Text]])
enumUsersOf uses =
  Map.fromListWith
    (\(_, new) (vs, old) -> (vs, old <> new))
    [(k, (vs, [path])) | (path, t) <- uses, (k, vs) <- enumsIn t]

-- | The enumerations, each named @Enum@: enough to render the other
-- modules, to find the names they declare.
placeholderEnums :: [([Text], HsType)] -> [EnumDef]
placeholderEnums uses = [EnumDef "Enum" k [] [] | k <- Map.keys (enumUsersOf uses)]

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
  dedupe [CConst (name <> suffix v) v | v <- vs, not (ekExtras k == boolExtras && v `Set.member` boolExtras)]
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
        Left d
          | d < 0 -> "Neg" <> decimal (negate d)
          | otherwise -> decimal d
      -- An axis type of "-" means plotly.js picks the type.
      VStr "-" | VStr "linear" `elem` vs -> "Auto"
      VStr ""
        | VStr "none" `elem` vs -> "Empty"
        | otherwise -> "None"
      VStr s
        | T.all (not . isAlphaNum) s -> T.concat (map symbolName (T.unpack s))
        | otherwise -> pascal s
    -- Without a sign or exponent, which an identifier cannot hold.
    decimal d = T.map (\c -> if c == '.' then '_' else c) (T.pack (showFFloat Nothing d ""))
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
