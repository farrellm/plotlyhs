{-# LANGUAGE OverloadedStrings #-}

-- | The names generated enumerations must not take, found in the code
-- rather than listed: the names plotlyhs's hand-written modules export,
-- the names the generator declares in its other modules, and the few
-- names from the Prelude and libraries that generated code uses.
--
-- Haskell source is read as plotlyhs formats it (with ormolu): an export
-- list in parentheses after the module name, and each declaration starting
-- in the first column. Comments, pragmas and the contents of literals are
-- skipped, and every branch of a CPP conditional is read, so that the names
-- do not depend on the platform. Anything else the reader does not
-- recognise is an error rather than skipped: a name it missed would show up
-- only later, as an ambiguous name in a hand-written module.
module PlotlyGen.Reserved
  ( Source (..),
    Export (..),
    parseSource,
    reservedNames,
    externalTypes,
  )
where

import Control.Monad (forM, unless)
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import Data.Containers.ListUtils (nubOrd)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | What a module declares and exports.
data Source = Source
  { srcModule :: Text,
    -- | Its export list, or 'Nothing' if it has none. Items that cannot
    -- name a type, class or constructor are left out.
    srcExports :: Maybe [Export],
    -- | Its types, type synonyms, type families and classes, each with its
    -- constructors.
    srcDecls :: [(Text, [Text])],
    -- | Its pattern synonyms.
    srcPatterns :: [Text]
  }

-- | An export list item that can name a type, class or constructor.
data Export
  = -- | @module M@.
    ExportModule Text
  | -- | A type or class; whether with all its constructors (@(..)@); and the
    -- constructors and pattern synonyms named with it.
    ExportType Text Bool [Text]
  | -- | @pattern P@.
    ExportPattern Text
  deriving stock (Eq, Show)

-- | A line number and what is wrong there.
type Err = (Int, String)

-- | Read a module's name, export list and declarations, or say where the
-- source has something the reader does not recognise.
parseSource :: FilePath -> Text -> Either String Source
parseSource path src = first (\(n, e) -> path <> ":" <> show n <> ": " <> e) $ do
  ls <- zip [1 ..] <$> clean src
  (start, header, body) <- case break (("module " `T.isPrefixOf`) . snd) ls of
    (before, m : rest)
      | (n, _) : _ <- filter (not . blankLine . snd) before -> Left (n, "expected the module header")
      | otherwise -> case break (endsHeader . snd) (m : rest) of
          (h, e : b) -> Right (fst m, h <> [e], b)
          _ -> Left (fst m, "expected `where` at the end of the module header")
    _ -> Left (1, "no module header")
  case [n | (n, l) <- header, "#" `T.isPrefixOf` l] of
    n : _ -> Left (n, "CPP in the module header")
    [] -> pure ()
  (name, exports) <- first (start,) (moduleHeader (T.unwords (map snd header)))
  body' <- conditionals body
  case filter (not . blankLine . snd) body' of
    (n, l) : _ | " " `T.isPrefixOf` l -> Left (n, "expected declarations in the first column")
    _ -> pure ()
  decls <- concat <$> mapM (uncurry declaration) (blocks body')
  let (types, patterns) = partitionEithers decls
  pure Source {srcModule = name, srcExports = exports, srcDecls = types, srcPatterns = nubOrd patterns}
  where
    endsHeader l = lastWord l == Just "where"

blankLine :: Text -> Bool
blankLine = T.all isSpace

lastWord :: Text -> Maybe Text
lastWord = listToMaybe . reverse . T.words

-- | The source's lines with comments and pragmas blanked out and the
-- contents of string and character literals replaced with underscores, so
-- that what is left is code, each line where it was.
clean :: Text -> Either Err [Text]
clean = fmap (T.lines . T.pack . reverse) . code 1 [] ' ' . T.unpack
  where
    -- The line, the output so far (reversed), the previous character and
    -- the rest of the input.
    code :: Int -> String -> Char -> String -> Either Err String
    code _ acc _ [] = Right acc
    code n acc p s@(c : cs)
      | "{-" `isPrefixOf` s = comment n n (1 :: Int) (blank 2 acc) (drop 2 s)
      | "--" `isPrefixOf` s,
        not (isSymbolChar p),
        not (any isSymbolChar (take 1 (dropWhile (== '-') s))) =
          let (line, rest) = break (== '\n') s
           in code n (blank (length line) acc) ' ' rest
      | c == '"' = string n n ('"' : acc) cs
      | c == '\'',
        not (isNameChar p),
        Just (len, rest) <- charLiteral cs =
          code n ('\'' : replicate len '_' <> ('\'' : acc)) '\'' rest
      | otherwise = code (if c == '\n' then n + 1 else n) (c : acc) c cs
    comment start n d acc = \case
      [] -> Left (start, "unterminated block comment")
      '{' : '-' : rest -> comment start n (d + 1) (blank 2 acc) rest
      '-' : '}' : rest
        | d == 1 -> code n (blank 2 acc) ' ' rest
        | otherwise -> comment start n (d - 1) (blank 2 acc) rest
      '\n' : rest -> comment start (n + 1) d ('\n' : acc) rest
      _ : rest -> comment start n d (' ' : acc) rest
    string start n acc = \case
      '"' : rest -> code n ('"' : acc) '"' rest
      '\\' : c : rest
        | isSpace c -> gap start n ('_' : acc) (c : rest)
        | c /= '\n' -> string start n ("__" <> acc) rest
      c : rest | c /= '\n' -> string start n ('_' : acc) rest
      _ -> Left (start, "unterminated string literal")
    -- A string gap: whitespace between backslashes.
    gap start n acc = \case
      '\\' : rest -> string start n ('_' : acc) rest
      '\n' : rest -> gap start (n + 1) ('\n' : acc) rest
      c : rest | isSpace c -> gap start n (' ' : acc) rest
      _ -> Left (start, "unterminated string gap")
    -- After a quote, the length of a character literal's contents and the
    -- input after it, or 'Nothing' for a promotion tick.
    charLiteral = \case
      '\\' : rest
        | (body, '\'' : rest') <- break (`elem` ("'\n" :: String)) (drop 1 rest) -> Just (2 + length body, rest')
      c : '\'' : rest | c `notElem` ("'\\\n" :: String) -> Just (1, rest)
      _ -> Nothing
    blank k acc = replicate k ' ' <> acc

-- | Blank out CPP conditionals, keeping every branch. Any other directive is
-- an error.
conditionals :: [(Int, Text)] -> Either Err [(Int, Text)]
conditionals = go []
  where
    -- The lines of the conditionals still open.
    go opened [] = case opened of
      [] -> Right []
      n : _ -> Left (n, "unterminated #if")
    go opened ((n, l) : rest) = case T.words <$> T.stripPrefix "#" l of
      Nothing -> ((n, l) :) <$> go opened rest
      Just ws -> do
        opened' <- case (ws, opened) of
          (w : _, _) | w `elem` ["if", "ifdef", "ifndef"] -> Right (n : opened)
          (w : _, _ : _) | w `elem` ["elif", "else"] -> Right opened
          ("endif" : _, _ : outer) -> Right outer
          _ -> Left (n, "cannot read CPP directive " <> show l)
        ((n, "") :) <$> go opened' rest

-- | The module's name and export list, from its header.
moduleHeader :: Text -> Either String (Text, Maybe [Export])
moduleHeader header = do
  let afterModule = T.strip (T.drop (T.length "module ") header)
      (name, rest) = T.break (\c -> isSpace c || c == '(') afterModule
      list = T.strip (fromMaybe rest (T.stripSuffix "where" (T.stripEnd rest)))
  unless (all isConName (T.splitOn "." name)) $ Left ("cannot read module name " <> show name)
  if T.null list
    then pure (name, Nothing)
    else case enclosed list of
      Just items -> (name,) . Just . catMaybes <$> mapM exportItem (filter (not . T.null) (map T.strip (splitTop ',' items)))
      Nothing -> Left ("cannot read the export list " <> show list)

-- | An export list item, or 'Nothing' for a value, or an operator without
-- constructors.
exportItem :: Text -> Either String (Maybe Export)
exportItem item = case T.words item of
  ["module", m] -> Right (Just (ExportModule m))
  "type" : _ -> thing (T.strip (T.drop 4 item))
  ["pattern", p]
    | isConName p -> Right (Just (ExportPattern p))
    | isOperator p -> Right Nothing
  _ -> thing item
  where
    thing t = case nameAndRest t of
      (name, "") | isVarName name || isOperator name -> Right Nothing
      (name, rest)
        | isConName name || isOperator name,
          Just withs <- if T.null rest then Just [] else map T.strip . splitTop ',' <$> enclosed rest,
          all (\w -> w == ".." || isConName w || isVarName w || isOperator w) withs ->
            Right (Just (ExportType (operatorName name) (".." `elem` withs) (filter isConName withs)))
      _ -> Left ("cannot read export " <> show item)
    -- The name, unqualified, and what follows it.
    nameAndRest t = case T.uncons t of
      Just ('(', _) ->
        let close = length (takeWhile (/= 0) (drop 1 (scanl (+) 0 (map depth (T.unpack t)))))
            (op, rest) = T.splitAt (close + 1) t
         in (op, T.strip rest)
      _ ->
        let (qualified, rest) = T.break (\c -> isSpace c || c == '(') t
         in (last (T.splitOn "." qualified), T.strip rest)

-- | Top-level declarations, with the line each starts on: a line in the
-- first column and the indented lines after it.
blocks :: [(Int, Text)] -> [(Int, [Text])]
blocks = go
  where
    go [] = []
    go ((n, l) : rest)
      | topLevel l = let (more, rest') = break (topLevel . snd) rest in (n, l : map snd more) : go rest'
      | otherwise = go rest
    topLevel l = not (blankLine l) && not (" " `T.isPrefixOf` l)

-- | The names a declaration declares at the type level, with its
-- constructors ('Left'), or the pattern synonyms it declares ('Right').
declaration :: Int -> [Text] -> Either Err [Either (Text, [Text]) Text]
declaration line block = first (line,) $ case T.words firstLine of
  "data" : "family" : n : _ -> named n []
  "data" : "instance" : _ -> Right []
  "data" : n : _ -> named n =<< ctors block
  "newtype" : "instance" : _ -> Right []
  "newtype" : n : _ -> named n =<< ctors block
  "type" : "family" : n : _ -> named n []
  "type" : w : _ | w `elem` ["role", "instance"] -> Right []
  "type" : "data" : n : _ -> named n =<< ctors block
  "type" : n : _ -> named n []
  "class" : _ -> (<>) <$> named (className (T.unwords block)) [] <*> (concat <$> mapM associated (drop 1 block))
  "pattern" : _ -> map Right <$> patternNames (T.drop (T.length "pattern") firstLine)
  w : _
    | w `elem` ["instance", "deriving", "import", "infix", "infixl", "infixr", "default", "foreign"] -> Right []
    -- A signature or a binding, not a Template Haskell splice.
    | Just (c, _) <- T.uncons w,
      isLower c || c `elem` ("_(" :: String),
      "::" `elem` topWords (T.unwords block) || length (splitTop '=' (T.unwords block)) > 1 ->
        Right []
  _ -> Left ("cannot read top-level declaration " <> show firstLine)
  where
    firstLine = fromMaybe "" (listToMaybe block)
    named n cs
      | isConName n || isOperator n = Right [Left (operatorName n, cs)]
      | otherwise = Left ("cannot read the name declared by " <> show firstLine)
    -- A class's associated type or data family.
    associated l = case T.words l of
      w : "family" : n : _ | w `elem` ["type", "data"] -> named n []
      w : n : _ | w `elem` ["type", "data"], n /= "instance" -> named n []
      _ -> Right []
    className t =
      let decl = fst (T.breakOn " where" t)
          afterContext = case T.breakOn "=>" decl of
            (_, rest) | not (T.null rest) -> T.drop 2 rest
            _ -> T.drop (T.length "class") decl
       in fromMaybe "" (listToMaybe (T.words (T.filter (`notElem` ("()" :: String)) afterContext)))

-- | The pattern synonyms declared by what follows @pattern@: @P, Q :: T@,
-- @P x <- ...@ or @x :> y = ...@.
patternNames :: Text -> Either String [Text]
patternNames decl = do
  let (lhs, sep) = break (`elem` ["::", "<-", "="]) (topWords decl)
      names = case (sep, infixCon lhs) of
        ("::" : _, _) -> map T.strip (T.splitOn "," (T.unwords lhs))
        (_, Just c) -> [c]
        _ | any isInfixCon lhs -> []
        _ -> take 1 lhs
  unless (all (\n -> isConName n || isOperator n) names) $
    Left ("cannot read pattern synonym " <> show (T.strip decl))
  pure (filter isConName names)

-- | The constructors of a @data@ or @newtype@ declaration: the
-- alternatives after its @=@, or in GADT syntax, the names before @::@ on
-- the lines after its @where@.
ctors :: [Text] -> Either String [Text]
ctors block = case splitTop '=' (T.unwords block) of
  hd : _ | "where" `elem` T.words hd -> gadt
  _ : alts : _ ->
    catMaybes
      <$> mapM
        alternative
        [alt | alt <- splitTop '|' (fst (T.breakOn " deriving " (alts <> " "))), not (blankLine alt)]
  _ -> Right []
  where
    alternative alt = case topWords (dropContext (dropForall (topWords alt))) of
      ws | Just c <- infixCon ws -> Right (Just c)
      ws | any isInfixCon ws -> Right Nothing
      c : _
        | isConName c -> Right (Just c)
        | isOperator c -> Right Nothing
      _ -> Left ("cannot read constructor " <> show (T.strip alt))
    dropForall = \case
      "forall" : rest -> drop 1 (dropWhile (not . T.isSuffixOf ".") rest)
      ws -> ws
    dropContext ws = T.unwords (case break (== "=>") ws of (_, _ : rest) -> rest; _ -> ws)
    gadt = case break (elem "where" . T.words) block of
      (_, w : rest)
        | lastWord w /= Just "where" -> Left "expected constructors on the lines after `where`"
        | otherwise ->
            let ls = filter (not . blankLine) rest
                indent l = T.length (T.takeWhile (== ' ') l)
                outer = minimum (map indent ls)
             in if null ls then Right [] else concat <$> mapM (con . T.strip) (filter ((== outer) . indent) ls)
      _ -> Right []
    con l
      | "deriving" `elem` take 1 (T.words l) = Right []
      | (cs, rest) <- T.breakOn "::" l,
        not (T.null rest),
        names <- map T.strip (T.splitOn "," cs),
        all (\c -> isConName c || isOperator c) names =
          Right (filter isConName names)
      | otherwise = Left ("cannot read GADT constructor " <> show l)

-- | The constructor of an infix declaration such as @a \`Con\` b@.
infixCon :: [Text] -> Maybe Text
infixCon ws = listToMaybe [c | w <- ws, Just c <- [T.stripPrefix "`" w >>= T.stripSuffix "`"], isConName c]

-- | A constructor operator, or a backquoted name.
isInfixCon :: Text -> Bool
isInfixCon w = ":" `T.isPrefixOf` w || "`" `T.isPrefixOf` w

-- | Words outside parentheses, brackets and braces.
topWords :: Text -> [Text]
topWords = filter (not . T.null) . map T.strip . splitTop ' '

-- | The text between a parenthesis at the start and the one matching it at
-- the end.
enclosed :: Text -> Maybe Text
enclosed t = do
  inner <- T.stripPrefix "(" t >>= T.stripSuffix ")"
  let depths = scanl (+) 0 (map depth (T.unpack inner))
  if all (>= 0) depths && last depths == 0 then Just inner else Nothing

-- | Split at a character outside parentheses, brackets and braces; @=@ and
-- @|@ count only alone, so that @=>@ and @||@ do not split.
splitTop :: Char -> Text -> [Text]
splitTop sep = map T.pack . go (0 :: Int) [] . T.unpack
  where
    go _ acc [] = [reverse acc]
    go d acc (c : cs)
      | d == 0, c == sep, alone acc cs = reverse acc : go d [] cs
      | otherwise = go (d + depth c) (c : acc) cs
    alone acc cs = sep `elem` (", " :: String) || (notOp (take 1 acc) && notOp (take 1 cs))
    notOp = all (`notElem` ("=|>-<:!" :: String))

depth :: Char -> Int
depth c
  | c `elem` ("([{" :: String) = 1
  | c `elem` (")]}" :: String) = -1
  | otherwise = 0

isSymbolChar :: Char -> Bool
isSymbolChar = (`elem` ("!#$%&*+./<=>?@\\^|-~:" :: String))

-- | A constructor, type or class name: not an operator.
isConName :: Text -> Bool
isConName n = case T.uncons n of
  Just (c, rest) -> isUpper c && T.all isNameChar rest
  Nothing -> False

-- | A variable name.
isVarName :: Text -> Bool
isVarName n = case T.uncons n of
  Just (c, rest) -> (isLower c || c == '_') && T.all isNameChar rest
  Nothing -> False

isNameChar :: Char -> Bool
isNameChar x = isAlphaNum x || x `elem` ("_'" :: String)

-- | An operator in parentheses.
isOperator :: Text -> Bool
isOperator n = case T.strip <$> enclosed n of
  Just op -> not (T.null op) && T.all isSymbolChar op
  Nothing -> False

-- | An operator without its parentheses.
operatorName :: Text -> Text
operatorName n = maybe n T.strip (enclosed n)

-- | Every name generated enumerations must not take, as types or as
-- constructors, with where it comes from. From the hand-written modules,
-- the names exported by those whose name has no @Internal@, following
-- @module@ re-exports among them and expanding @Name (..)@ with the
-- constructors declared anywhere, hand-written, generated or in
-- 'externalTypes'. From the generated modules, every name declared. A
-- re-export of a module that is neither, or @Name (..)@ for a type declared
-- nowhere, is an error, since the names it exports are unknown.
reservedNames :: [Source] -> [Source] -> Either String (Map Text Text)
reservedNames handWritten generated = do
  public <- forM [s | s <- handWritten, "Internal" `notElem` T.splitOn "." (srcModule s)] $ \s ->
    map (,srcModule s) <$> exported Set.empty s
  pure . Map.filterWithKey (\n _ -> isConName n) . Map.unions $
    [ Map.fromList [(n, "the Prelude or a library") | (t, cs) <- externalTypes, n <- t : cs],
      Map.fromList [(n, srcModule s) | s <- generated, n <- declared s],
      Map.fromList (concat public)
    ]
  where
    byName = Map.fromList [(srcModule s, s) | s <- handWritten]
    generatedNames = Set.fromList (map srcModule generated)
    ctorsOf = Map.fromListWith (<>) (externalTypes <> [(t, cs) | s <- handWritten <> generated, (t, cs) <- srcDecls s])
    declared s = [n | (t, cs) <- srcDecls s, n <- t : cs] <> srcPatterns s
    exported seen s = case srcExports s of
      Nothing -> Right (declared s)
      Just items -> concat <$> mapM (item s (Set.insert (srcModule s) seen)) items
    item s seen = \case
      ExportModule m
        | m `Set.member` seen || m `Set.member` generatedNames -> Right []
        | Just s' <- Map.lookup m byName -> exported seen s'
        | otherwise -> Left (T.unpack (srcModule s) <> " re-exports module " <> T.unpack m <> ", whose names are unknown")
      ExportPattern p -> Right [p]
      ExportType t withAll withs
        | not withAll -> Right (t : withs)
        | Just cs <- Map.lookup t ctorsOf -> Right (t : cs <> withs)
        | otherwise -> Left (T.unpack (srcModule s) <> " exports " <> T.unpack t <> " (..), but no module declares " <> T.unpack t)

-- | The Prelude and library types that generated code uses or hand-written
-- modules re-export, with the constructors it uses or they re-export.
externalTypes :: [(Text, [Text])]
externalTypes =
  [ ("Bool", ["False", "True"]),
    ("Double", []),
    ("Either", ["Left", "Right"]),
    ("Int", []),
    ("Maybe", ["Nothing", "Just"]),
    ("NonEmpty", [":|"]),
    ("Ordering", []),
    ("Text", []),
    ("Type", []),
    ("Value", []),
    ("Void", [])
  ]
