{-# LANGUAGE OverloadedStrings #-}

-- | The names generated enumerations must not take, found in the code
-- rather than listed: the names plotlyhs's hand-written modules export,
-- the names the generator declares in its other modules, and the few
-- names from the Prelude and libraries that generated code uses.
--
-- Haskell source is read as plotlyhs formats it (with ormolu): an export
-- list in parentheses after the module name, and each declaration starting
-- in the first column.
module PlotlyGen.Reserved
  ( Source (..),
    parseSource,
    reservedNames,
    externalNames,
  )
where

import Data.Char (isAlphaNum, isUpper)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | What a module declares and exports.
data Source = Source
  { srcModule :: Text,
    -- | The items of its export list, or 'Nothing' if it has none.
    srcExports :: Maybe [Text],
    -- | Its types, type synonyms, type families and classes, each with its
    -- constructors.
    srcDecls :: [(Text, [Text])]
  }

-- | Read a module's name, export list and declarations.
parseSource :: Text -> Source
parseSource src =
  Source
    { srcModule = case T.words headerText of
        _ : name : _ -> T.takeWhile (/= '(') name
        _ -> "",
      srcExports = exportsOf headerText,
      srcDecls = concatMap declaration (blocks body)
    }
  where
    ls = map stripComment (T.lines src)
    (header, body) = case break ("module " `T.isPrefixOf`) ls of
      (_, m : rest) -> let (h, b) = break endsHeader (m : rest) in (h <> take 1 b, drop 1 b)
      _ -> ([], ls)
    endsHeader l = T.strip l == "where" || " where" `T.isSuffixOf` T.stripEnd l
    headerText = T.unwords header

-- | The items between the parentheses after the module name.
exportsOf :: Text -> Maybe [Text]
exportsOf header = case T.breakOn "(" (T.unwords (drop 2 (T.words header))) of
  (before, rest)
    | T.null rest || not (T.null (T.strip before)) -> Nothing
    | otherwise -> Just (filter (not . T.null) (map T.strip (splitTop ',' (inner (T.drop 1 rest)))))
  where
    -- Up to the matching parenthesis.
    inner = T.pack . go (0 :: Int) . T.unpack
      where
        go _ [] = []
        go 0 (')' : _) = []
        go d (c : cs) = c : go (d + depth c) cs

-- | A Haskell line without its comment.
stripComment :: Text -> Text
stripComment l = case T.breakOn "--" l of
  (before, rest)
    | T.null rest -> l
    | starts before && ends (T.drop 2 rest) -> before
    | otherwise -> before <> T.take 2 rest <> stripComment (T.drop 2 rest)
  where
    starts b = T.null b || T.last b == ' '
    ends r = T.null r || T.head r `elem` (" |^*-" :: String)

-- | Top-level declarations: a line in the first column and the indented
-- lines after it.
blocks :: [Text] -> [[Text]]
blocks = go
  where
    go [] = []
    go (l : rest)
      | topLevel l = let (more, rest') = break topLevel rest in (l : more) : go rest'
      | otherwise = go rest
    topLevel l = not (T.null l) && T.head l /= ' '

-- | The names a declaration declares at the type level, with its
-- constructors.
declaration :: [Text] -> [(Text, [Text])]
declaration block = case T.words (fromMaybe "" (listToMaybe block)) of
  "data" : "family" : n : _ -> named n []
  "data" : "instance" : _ -> []
  "data" : n : _ -> named n (ctors block)
  "newtype" : "instance" : _ -> []
  "newtype" : n : _ -> named n (ctors block)
  "type" : "family" : n : _ -> named n []
  "type" : w : _ | w `elem` ["role", "instance"] -> []
  "type" : n : _ -> named n []
  "class" : _ -> named (className (T.unwords block)) [] <> concatMap associated (drop 1 block)
  _ -> []
  where
    named n cs = [(n, cs) | isConName n]
    -- A class's associated type or data family.
    associated l = case T.words l of
      w : "family" : n : _ | w `elem` ["type", "data"] -> named n []
      w : n : _ | w `elem` ["type", "data"], n /= "instance" -> named n []
      _ -> []
    className t =
      let decl = fst (T.breakOn " where" t)
          afterContext = case T.breakOn "=>" decl of
            (_, rest) | not (T.null rest) -> T.drop 2 rest
            _ -> T.drop 5 decl
       in fromMaybe "" (listToMaybe (T.words (T.filter (`notElem` ("()" :: String)) afterContext)))

-- | The constructors of a @data@ or @newtype@ declaration: the
-- alternatives after its @=@, or in GADT syntax, the names before @::@ on
-- the lines after its @where@.
ctors :: [Text] -> [Text]
ctors block = case splitTop '=' (T.unwords block) of
  hd : _ | "where" `elem` T.words hd -> gadt
  _ : alts : _ ->
    [ c
    | alt <- splitTop '|' (fst (T.breakOn " deriving " (alts <> " "))),
      c : _ <- [T.words alt],
      isConName c
    ]
  _ -> []
  where
    gadt =
      [ c
      | l <- drop 1 block,
        (cs, rest) <- [T.breakOn "::" l],
        not (T.null rest),
        c <- map T.strip (T.splitOn "," cs),
        isConName c
      ]

-- | Split at a character outside parentheses, brackets and braces; @=@ and
-- @|@ count only alone, so that @=>@ and @||@ do not split.
splitTop :: Char -> Text -> [Text]
splitTop sep = map T.pack . go (0 :: Int) [] . T.unpack
  where
    go _ acc [] = [reverse acc]
    go d acc (c : cs)
      | d == 0, c == sep, alone acc cs = reverse acc : go d [] cs
      | otherwise = go (d + depth c) (c : acc) cs
    alone acc cs = sep == ',' || (notOp (take 1 acc) && notOp (take 1 cs))
    notOp = all (`notElem` ("=|>-<:!" :: String))

depth :: Char -> Int
depth c
  | c `elem` ("([{" :: String) = 1
  | c `elem` (")]}" :: String) = -1
  | otherwise = 0

-- | A constructor, type or class name: not an operator.
isConName :: Text -> Bool
isConName n = case T.uncons n of
  Just (c, rest) -> isUpper c && T.all (\x -> isAlphaNum x || x `elem` ("_'" :: String)) rest
  Nothing -> False

-- | Every name generated enumerations must not take, as types or as
-- constructors, with where it comes from. From the hand-written modules,
-- the names exported by those whose name has no @Internal@, following
-- @module@ re-exports among them and expanding @Name (..)@ with the
-- constructors declared anywhere, hand-written or generated. From the
-- generated modules, every name declared.
reservedNames :: [Source] -> [Source] -> Map Text Text
reservedNames handWritten generated =
  Map.unions
    [ Map.fromList [(n, "the Prelude or a library") | n <- externalNames],
      Map.fromList [(n, srcModule s) | s <- generated, (t, cs) <- srcDecls s, n <- t : cs],
      Map.fromList [(n, srcModule s) | s <- handWritten, "Internal" `notElem` T.splitOn "." (srcModule s), n <- exported Set.empty s]
    ]
  where
    byName = Map.fromList [(srcModule s, s) | s <- handWritten]
    ctorsOf = Map.fromListWith (<>) [(t, cs) | s <- handWritten <> generated, (t, cs) <- srcDecls s]
    exported seen s = case srcExports s of
      Nothing -> [n | (t, cs) <- srcDecls s, n <- t : cs]
      Just items -> concatMap (item (Set.insert (srcModule s) seen)) items
    item seen i = case T.words i of
      ["module", m]
        | m `Set.notMember` seen, Just s <- Map.lookup m byName -> exported seen s
        | otherwise -> []
      n : rest
        | isConName n -> n : subordinates n (T.concat rest)
      _ -> []
    subordinates n = \case
      "(..)" -> Map.findWithDefault [] n ctorsOf
      list | Just inner <- T.stripPrefix "(" list >>= T.stripSuffix ")" -> filter isConName (map T.strip (T.splitOn "," inner))
      _ -> []

-- | The Prelude and library names that generated code uses.
externalNames :: [Text]
externalNames = ["Bool", "Double", "Either", "Int", "Maybe", "Ordering", "Text", "Type", "Value", "Void", "False", "True", "Nothing", "Just", "Left", "Right"]
