{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Reads plotly.js's @plot-schema.json@ into a tree of objects and typed
-- attributes.
module PlotlyGen.Schema
  ( Schema (..),
    TraceDef (..),
    TraceSubplot (..),
    NamedKindDef (..),
    ObjDef (..),
    Child (..),
    Leaf (..),
    HsType (..),
    AxisDim (..),
    EnumVal (..),
    Payload (..),
    EnumKey (..),
    readSchema,
    objPathText,
    allObjects,
    objectsIn,
    allLeaves,
    enumsIn,
    boolExtras,
  )
where

import Control.Monad (forM, guard, unless, when)
import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isLetter)
import Data.List (find, nub, (\\))
import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data Schema = Schema
  { schemaTraces :: [TraceDef],
    -- | The kinds of subplot with ids, in 'namedKindNames' order.
    schemaNamedKinds :: [NamedKindDef],
    schemaLayout :: ObjDef,
    schemaConfig :: ObjDef,
    schemaFrame :: ObjDef,
    schemaAnimation :: ObjDef
  }

data TraceDef = TraceDef
  { -- | The trace's @type@, e.g. @scatter@.
    traceName :: Text,
    traceDoc :: Maybe Text,
    -- | The kind of subplot it is drawn on; 'Nothing' for a trace with no
    -- subplot of its own (@splom@ makes its own axes).
    traceSubplot :: Maybe TraceSubplot,
    traceAttrs :: ObjDef
  }

-- | A kind of subplot.
data TraceSubplot
  = -- | Cartesian x and y axes.
    OnXY
  | -- | A rectangle of the plot area, the trace's @domain@.
    OnDomain
  | -- | A kind whose subplots have ids.
    OnNamed NamedKindDef

-- | A kind of subplot whose subplots have ids (@scene@, @scene2@, …).
data NamedKindDef = NamedKindDef
  { -- | The layout key of its subplots, and their ids' prefix: @scene@.
    nkKey :: Text,
    -- | The trace attribute naming a trace's subplot: the key, or
    -- @subplot@, whose default is the key.
    nkAttr :: Text,
    -- | The name of a grid cell holding one: @Scene@.
    nkCell :: Text,
    -- | Haddock text, a noun phrase: @a 3D \@scene\@@.
    nkDoc :: Text
  }

-- | What the schema doesn't say about the kinds of subplot with ids: each
-- one's layout key, cell name ('nkCell') and documentation ('nkDoc'). The
-- schema's subplot objects, other than those in 'notNamedKinds', must be
-- exactly these keys.
namedKindNames :: [(Text, Text, Text)]
namedKindNames =
  [ ("scene", "Scene", "a 3D @scene@"),
    ("geo", "Geo", "a @geo@ map"),
    ("polar", "Polar", "a @polar@ subplot"),
    ("ternary", "Ternary", "a @ternary@ subplot"),
    -- Not Map, which would clash with Data.Map.
    ("map", "TileMap", "a tile @map@"),
    ("smith", "Smith", "a @smith@ chart")
  ]

-- | Layout objects the schema marks as subplot objects that aren't kinds of
-- subplot with ids: the cartesian axes, and objects that only share their
-- numbering (@coloraxis2@, @legend2@).
notNamedKinds :: [Text]
notNamedKinds = ["xaxis", "yaxis", "coloraxis", "legend"]

-- | An object in the attribute tree. Its path names it: @["scatter", "marker"]@.
data ObjDef = ObjDef
  { objPath :: [Text],
    objChildren :: [Child]
  }

data Child
  = -- | An attribute holding a value.
    CLeaf Text Leaf
  | -- | A nested object. 'True' marks a subplot object (@xaxis@, @scene@,
    -- …), which may also appear as @xaxis2@, @xaxis3@, ….
    CObject Text (Maybe Text) Bool ObjDef
  | -- | An array of objects, such as @layout.annotations@.
    CArray Text (Maybe Text) ObjDef

data Leaf = Leaf
  { leafType :: HsType,
    leafDoc :: Maybe Text,
    leafDefault :: Maybe Value,
    leafMin :: Maybe Value,
    leafMax :: Maybe Value
  }

-- | The Haskell type of an attribute's value.
data HsType
  = TDouble
  | TInt
  | TBool
  | TText
  | TValue
  | TColor
  | TColorscale
  | TDataArray
  | TSubplotId
  | -- | A cartesian axis: a trace's @xaxis@ or @yaxis@.
    TAxisId AxisDim
  | -- | An @xref@ or @yref@: the paper, an axis, or an axis's domain.
    TAxisRef AxisDim
  | TAngle
  | TEnum EnumKey [EnumVal]
  | TFlags EnumKey [EnumVal]
  | TArrayOk HsType
  | TTuple [HsType]
  | TList HsType
  | -- | A sequence of objects, reached with @objects@: traces, which are
    -- added at the end one at a time.
    TSeq HsType
  | -- | A type named outright, for the few attributes typed by hand.
    TNamed Text
  deriving stock (Eq, Show)

-- | Which cartesian axes an 'TAxisId' or 'TAxisRef' is of.
data AxisDim = AxisX | AxisY
  deriving stock (Eq, Ord, Show)

data EnumVal = VBool Bool | VNum Scientific | VStr Text
  deriving stock (Eq, Ord, Show)

-- | The non-constant alternative of an enumeration.
data Payload
  = -- | A subplot id matching one of the schema's regular expressions.
    PSubplot
  | -- | An integer, for an @integer@ attribute with @extras@.
    PInt
  | -- | A number, for a @number@ attribute with @extras@.
    PNumber
  deriving stock (Eq, Ord, Show)

-- | Identifies an enumeration: the same values and payload, used anywhere in
-- the schema, give the same Haskell type.
data EnumKey = EnumKey
  { ekFlags :: Bool,
    ekValues :: Set EnumVal,
    -- | For a flag list, the values that stand alone rather than combine.
    ekExtras :: Set EnumVal,
    ekPayload :: Maybe Payload
  }
  deriving stock (Eq, Ord, Show)

objPathText :: ObjDef -> Text
objPathText = T.intercalate "." . objPath

readSchema :: Value -> Either String Schema
readSchema schema = do
  traces <- objectAt ["traces"] schema
  layoutAttrs <- objectAt ["layout", "layoutAttributes"] schema
  configAttrs <- objectAt ["config"] schema
  frameAttrs <- objectAt ["frames", "items", "frames_entry"] schema
  animationAttrs <- objectAt ["animation"] schema
  let schemaKinds =
        [ Key.toText k
        | (k, Object v) <- KM.toList layoutAttrs,
          KM.lookup "_isSubplotObj" v == Just (Bool True)
        ]
          \\ notNamedKinds
      kindKeys = [key | (key, _, _) <- namedKindNames]
  unless (null (schemaKinds \\ kindKeys)) $
    Left ("subplot kinds missing from namedKindNames: " <> T.unpack (T.unwords (schemaKinds \\ kindKeys)))
  unless (null (kindKeys \\ schemaKinds)) $
    Left ("namedKindNames lists kinds the schema doesn't have: " <> T.unpack (T.unwords (kindKeys \\ schemaKinds)))
  placed <- forM (KM.toList traces) $ \(k, v) -> do
    let name = Key.toText k
    attrs <- objectAt ["attributes"] v
    placement <- tracePlacement kindKeys name attrs
    pure (name, v, attrs, placement)
  kinds <- forM namedKindNames $ \(key, cell, doc) ->
    case nub [a | (_, _, _, Just (PlaceNamed k a)) <- placed, k == key] of
      [a] -> pure (NamedKindDef key a cell doc)
      [] -> Left ("no trace is drawn on a " <> T.unpack key <> " subplot")
      as -> Left ("traces name their " <> T.unpack key <> " subplot with different attributes: " <> T.unpack (T.unwords as))
  traceDefs <- forM placed $ \(name, v, attrs, placement) -> do
    obj <- parseObj [name] attrs
    pure
      TraceDef
        { traceName = name,
          traceDoc = either (const Nothing) Just (objectAt ["meta"] v) >>= textAt "description",
          traceSubplot =
            placement >>= \case
              PlaceXY -> Just OnXY
              PlaceDomain -> Just OnDomain
              PlaceNamed key _ -> OnNamed <$> find ((== key) . nkKey) kinds,
          traceAttrs = obj
        }
  -- Trace types add layout attributes (barmode, boxgap, …); barpolar's
  -- belong to each polar subplot.
  let traceLayoutAttrs dest =
        [ (Key.toText k, a)
        | (t, v) <- KM.toList traces,
          ["polar" | Key.toText t == "barpolar"] == (dest :: [Text]),
          Right o <- [objectAt ["layoutAttributes"] v],
          (k, a) <- KM.toList o
        ]
  layoutAttrs' <- mergeAttrs [] layoutAttrs (traceLayoutAttrs [])
  layoutAttrs'' <- case KM.lookup "polar" layoutAttrs' of
    Just (Object polar) -> do
      polar' <- mergeAttrs ["polar"] polar (traceLayoutAttrs ["polar"])
      pure (KM.insert "polar" (Object polar') layoutAttrs')
    _ -> Left "layout has no polar object"
  layout <- overrideLayout <$> parseObj ["layout"] layoutAttrs''
  config <- parseObj ["config"] configAttrs
  frame <- overrideFrame <$> parseObj ["frame"] frameAttrs
  animation <- parseObj ["animation"] animationAttrs
  let s = Schema traceDefs kinds layout config frame animation
      paths = map objPath (allObjects s)
  when (length (nub paths) /= length paths) $
    Left "two objects share a path"
  pure s

-- | Add trace-contributed attributes to an object. An attribute contributed
-- twice must be defined the same way both times.
mergeAttrs :: [Text] -> KeyMap Value -> [(Text, Value)] -> Either String (KeyMap Value)
mergeAttrs path = foldl step . Right
  where
    step acc (k, v) = do
      o <- acc
      case KM.lookup (Key.fromText k) o of
        Nothing -> pure (KM.insert (Key.fromText k) v o)
        Just v' -> do
          unless (withoutDoc v == withoutDoc v') $
            Left ("conflicting definitions of layout attribute " <> T.unpack (T.intercalate "." (path <> [k])))
          pure o
    withoutDoc (Object o) = Object (KM.delete "description" o)
    withoutDoc v = v

-- | How a trace is placed on a subplot.
data Placement
  = PlaceXY
  | PlaceDomain
  | -- | On a kind with ids, given by its layout key, named by this trace
    -- attribute.
    PlaceNamed Text Text

-- | How a trace is placed, from the attributes that place it: @xaxis@ and
-- @yaxis@; an attribute named after a kind with ids (@scene@), or
-- @subplot@, whose default is the kind (@polar@); or @domain@. A trace with
-- none of them (@splom@) has no subplot.
tracePlacement :: [Text] -> Text -> KeyMap Value -> Either String (Maybe Placement)
tracePlacement kindKeys name attrs
  | has "xaxis" && has "yaxis" = Right (Just PlaceXY)
  | otherwise = case named of
      [(key, attr)] -> Right (Just (PlaceNamed key attr))
      _ : _ : _ -> Left ("trace " <> T.unpack name <> " names more than one kind of subplot")
      []
        | has "subplot" -> Left ("unknown subplot kind " <> show subplotDefault <> " for trace " <> T.unpack name)
        | has "domain" -> Right (Just PlaceDomain)
        | otherwise -> Right Nothing
  where
    has k = KM.member (Key.fromText k) attrs
    subplotDefault = case KM.lookup "subplot" attrs of
      Just (Object o) -> textAt "dflt" o
      _ -> Nothing
    named =
      [(k, k) | k <- kindKeys, has k]
        <> [(k, "subplot") | Just k <- [subplotDefault], k `elem` kindKeys]

-- | The layout's @template@ is typed as @any@ in the schema; it is a
-- template.
overrideLayout :: ObjDef -> ObjDef
overrideLayout o = o {objChildren = map go (objChildren o)}
  where
    go (CLeaf "template" l) = CLeaf "template" l {leafType = TNamed "Template"}
    go c = c

-- | A frame's @data@ and @layout@ are typed as @any@ in the schema; they are
-- traces and a layout.
overrideFrame :: ObjDef -> ObjDef
overrideFrame o = o {objChildren = map go (objChildren o)}
  where
    go (CLeaf k l)
      | Just t <- lookup k overrides = CLeaf k l {leafType = t}
    go c = c
    overrides =
      [ ("data", TSeq (TNamed "SomeTrace")),
        ("layout", TNamed "Layout"),
        ("traces", TList TInt)
      ]

objectAt :: [Text] -> Value -> Either String (KeyMap Value)
objectAt [] (Object o) = Right o
objectAt (k : ks) (Object o)
  | Just v <- KM.lookup (Key.fromText k) o = objectAt ks v
objectAt ks _ = Left ("schema has no object at " <> T.unpack (T.intercalate "." ks))

textAt :: Text -> KeyMap Value -> Maybe Text
textAt k o = case KM.lookup (Key.fromText k) o of
  Just (String t) -> Just t
  _ -> Nothing

-- | Parse an attribute object. Non-object entries (@role@, @editType@,
-- @description@), @impliedEdits@ and keys starting with @_@ are metadata.
parseObj :: [Text] -> KeyMap Value -> Either String ObjDef
parseObj path o = do
  children <- forM [(Key.toText k, v) | (k, Object v) <- KM.toList o] $ \(k, v) ->
    if
      | "_" `T.isPrefixOf` k || k == "impliedEdits" -> pure Nothing
      | KM.member "valType" v -> Just . CLeaf k <$> parseLeaf (path <> [k]) v
      | Just (Object items) <- KM.lookup "items" v -> case [(Key.toText ik, iv) | (ik, Object iv) <- KM.toList items] of
          [(ik, iv)] -> Just . CArray k (textAt "description" v) <$> parseObj (path <> [ik]) iv
          _ -> Left ("expected one item type in " <> T.unpack (T.intercalate "." (path <> [k])))
      | otherwise ->
          Just . CObject k (textAt "description" v) (KM.lookup "_isSubplotObj" v == Just (Bool True))
            <$> parseObj (path <> [k]) v
  pure (ObjDef path (catMaybes children))

parseLeaf :: [Text] -> KeyMap Value -> Either String Leaf
parseLeaf path o = do
  t <- valueType path o
  pure
    Leaf
      { leafType = t,
        leafDoc = textAt "description" o,
        leafDefault = KM.lookup "dflt" o,
        leafMin = KM.lookup "min" o,
        leafMax = KM.lookup "max" o
      }

valueType :: [Text] -> KeyMap Value -> Either String HsType
valueType path o = do
  base <- case textAt "valType" o of
    Just "data_array" -> pure TDataArray
    Just "enumerated" -> enumerated path =<< listAt "values"
    Just "flaglist" -> do
      fs <- listAt "flags"
      let es = either (const []) (map toEnumVal) (listAt "extras")
          vals = map toEnumVal fs <> es
          isStr = \case VStr _ -> True; _ -> False
      unless (all (isStr . toEnumVal) fs) $
        Left ("non-string flags at " <> T.unpack (T.intercalate "." path))
      unless (all isStr es || Set.fromList es == boolExtras) $
        Left ("extras mixing booleans and strings at " <> T.unpack (T.intercalate "." path))
      -- With no flags, only one value can be given: an enumeration.
      pure $
        if null fs
          then enumWith Nothing es
          else TFlags (EnumKey True (Set.fromList vals) (Set.fromList es) Nothing) vals
    Just "color" -> pure TColor
    Just "colorlist" -> pure (TList TColor)
    Just "colorscale" -> pure TColorscale
    Just "number" -> pure (withExtras PNumber TDouble)
    Just "integer" -> pure (withExtras PInt TInt)
    Just "boolean" -> pure TBool
    Just "string" -> pure TText
    Just "angle" -> pure TAngle
    Just "subplotid" -> pure $ case KM.lookup "dflt" o of
      Just (String "x") -> TAxisId AxisX
      Just (String "y") -> TAxisId AxisY
      _ -> TSubplotId
    Just "any" -> pure TValue
    Just "info_array" -> infoArray
    t -> Left ("unknown valType " <> show t <> " at " <> T.unpack (T.intercalate "." path))
  pure $
    if KM.lookup "arrayOk" o == Just (Bool True) && base /= TValue
      then TArrayOk base
      else base
  where
    listAt k = case KM.lookup k o of
      Just (Array a) -> Right (foldr (:) [] a)
      _ -> Left ("no " <> T.unpack (Key.toText k) <> " at " <> T.unpack (T.intercalate "." path))
    withExtras payload t = case KM.lookup "extras" o of
      Just (Array a) | not (null a) -> enumWith (Just payload) (map toEnumVal (foldr (:) [] a))
      _ -> t
    infoArray = case KM.lookup "items" o of
      Just (Object item) -> do
        t <- valueType path item
        pure $ case KM.lookup "dimensions" o of
          Just (Number 2) -> TList (TList t)
          Just _ -> TValue
          Nothing -> TList t
      Just (Array items) -> do
        ts <- forM (foldr (:) [] items) $ \case
          Object item -> valueType path item
          _ -> Left ("bad info_array item at " <> T.unpack (T.intercalate "." path))
        let free = KM.lookup "freeLength" o == Just (Bool True)
        pure $ case nub ts of
          _ | not free && length ts `elem` [2 .. 4] -> TTuple ts
          [t] -> TList t
          _ -> TList TValue
      _ -> Left ("info_array without items at " <> T.unpack (T.intercalate "." path))

-- | An enumeration's type. Regular expressions (@/^x([2-9]|…)$/@) stand for
-- subplot ids; @"paper"@ and one regular expression are an @xref@ or
-- @yref@, whose expression must be one letter's axes or their domains.
-- Numbers that alias names (@marker.symbol@'s @0@ and @"0"@ for
-- @"circle"@) are dropped.
enumerated :: [Text] -> [Value] -> Either String HsType
enumerated path vs
  | [VStr "paper"] <- consts,
    [r] <- regexes = case axisRegex r of
      Just d -> pure (TAxisRef d)
      Nothing -> Left ("an axis reference at " <> T.unpack (T.intercalate "." path) <> " with an unknown regular expression: " <> T.unpack r)
  | null consts && not (null regexes) = pure TSubplotId
  | otherwise = pure (enumWith (if null regexes then Nothing else Just PSubplot) consts')
  where
    (regexes, consts) = foldr split ([], []) vs
    split (String s) (rs, cs) | "/^" `T.isPrefixOf` s = (s : rs, cs)
    split v (rs, cs) = (rs, toEnumVal v : cs)
    named = [v | v@(VStr s) <- consts, not (isNumeric s)]
    consts'
      | null named = consts
      | otherwise = filter (\case VNum _ -> False; VStr s -> not (isNumeric s); VBool _ -> True) consts
    isNumeric s = not (T.null s) && T.all (`elem` ("0123456789" :: String)) s

-- | The dimension of a regular expression for one letter's axes, numbered
-- in any way, or their domains: @/^x([2-9]|[1-9][0-9]+)?( domain)?$/@.
axisRegex :: Text -> Maybe AxisDim
axisRegex r = do
  body <- T.stripSuffix "( domain)?$/" =<< T.stripPrefix "/^" r
  (c, number) <- T.uncons body
  d <- lookup c [('x', AxisX), ('y', AxisY)]
  d <$ guard (not (T.any isLetter number))

enumWith :: Maybe Payload -> [EnumVal] -> HsType
enumWith payload vals = TEnum (EnumKey False (Set.fromList vals) Set.empty payload) vals

toEnumVal :: Value -> EnumVal
toEnumVal (Bool b) = VBool b
toEnumVal (Number n) = VNum n
toEnumVal (String s) = VStr s
toEnumVal v = VStr (T.pack (show v))

-- | Extras of @true@ and @false@, which become a 'Bool'.
boolExtras :: Set EnumVal
boolExtras = Set.fromList [VBool True, VBool False]

-- | Every object in the schema, roots included.
allObjects :: Schema -> [ObjDef]
allObjects s =
  concatMap (objectsIn . traceAttrs) (schemaTraces s)
    <> concatMap objectsIn [schemaLayout s, schemaConfig s, schemaFrame s, schemaAnimation s]

-- | An object and every object beneath it, in pre-order, children in
-- declaration order.
objectsIn :: ObjDef -> [ObjDef]
objectsIn o = o : concatMap objectsIn (concatMap sub (objChildren o))
  where
    sub (CObject _ _ _ c) = [c]
    sub (CArray _ _ c) = [c]
    sub _ = []

-- | Every attribute in the schema, with its path.
allLeaves :: Schema -> [([Text], Leaf)]
allLeaves s = [(objPath o <> [k], l) | o <- allObjects s, CLeaf k l <- objChildren o]

-- | The enumerations a type mentions.
enumsIn :: HsType -> [(EnumKey, [EnumVal])]
enumsIn = \case
  TEnum k vs -> [(k, vs)]
  TFlags k vs -> [(k, vs)]
  TArrayOk t -> enumsIn t
  TTuple ts -> concatMap enumsIn ts
  TList t -> enumsIn t
  TSeq t -> enumsIn t
  _ -> []
