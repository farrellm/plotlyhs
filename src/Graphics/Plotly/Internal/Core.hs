{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The core types, with the internals that "Graphics.Plotly.Core" hides.
module Graphics.Plotly.Internal.Core
  ( -- * Figures
    Figure (..),
    figure,
    forgetGrid,
    FigureGrid (..),

    -- ** Subplot grids, as values
    FigureGridInfo (..),
    gridInfo,
    GridInfo (..),
    CellInfo (..),
    SubplotRef (..),
    StartCell (..),
    gridCellInfo,
    cellRefs,
    cellRefFor,
    CellIx (..),

    -- * Traces
    Trace (..),
    newTrace,
    TraceType (..),
    KnownTraceType (..),
    traceTypeName,
    traceTypeFromName,
    SubplotKind (..),
    NamedKind (..),
    traceSubplotKind,
    subplotName,
    subplotAttr,

    -- ** Traces of any type
    SomeTrace (..),
    someTrace,
    someTraceType,
    fromSomeTrace,
    rawTrace,

    -- * Objects
    Obj (..),
    emptyObj,
    Layout,
    Config,
    Frame,
    Animation,
    XAxis,
    YAxis,
    Shape,
    Annotation,
    Slider,
    SliderStep,
    Updatemenu,
    Button,

    -- * Untyped access
    IsObject,
    toObject,
    fromObject,
    attr,
    attrKeys,

    -- * Building optics
    field,
    nested,
    objects,
    subplotKey,
  )
where

import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.List ((!?))
import Data.Maybe (maybeToList)
import Data.Proxy (Proxy (..))
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Data.Text qualified as T
import Data.Type.Coercion (Coercion (..), coerceWith, sym)
import Data.Vector qualified as V
import GHC.TypeLits (Symbol)
import Graphics.Plotly.Grid (CellSpec, FigureGrid (..))
import Graphics.Plotly.SubplotKind (NamedKind (..), SubplotKind (..), subplotAttr, subplotName)
import Graphics.Plotly.TraceType
import Graphics.Plotly.Types (AxisId, Dim (..), subplotIdText)
import Optics.Core

-- | A trace of type @t@: @Trace Scatter@, @Trace Bar@, ….
newtype Trace (t :: TraceType) = Trace Fields
  deriving stock (Eq, Show)

type role Trace nominal

-- | A trace with only its type set. Each trace module exports one under the
-- trace type's name: 'Graphics.Plotly.Trace.Scatter.scatter', ….
newTrace :: forall t. (KnownTraceType t) => Trace t
newTrace = Trace (Fields (KM.singleton "type" (Leaf (String (traceTypeName (traceTypeOf (Proxy @t)))))))

instance ToJSON (Trace t) where
  toJSON = Object . toObject

-- | Fails if the @type@ is not @t@.
instance (KnownTraceType t) => FromJSON (Trace t) where
  parseJSON = withObject "Trace" $ \o -> case fromSomeTrace (fromObject o) of
    Just tr -> pure tr
    Nothing -> fail ("Trace: not a " <> T.unpack (traceTypeName (traceTypeOf (Proxy @t))) <> " trace")

-- | A trace of any type, as a figure holds them.
newtype SomeTrace = SomeTrace Fields
  deriving stock (Eq, Show)

instance ToJSON SomeTrace where
  toJSON = Object . toObject

instance FromJSON SomeTrace where
  parseJSON = withObject "SomeTrace" (pure . fromObject)

-- | Forget a trace's type, to put it in a figure.
someTrace :: Trace t -> SomeTrace
someTrace (Trace o) = SomeTrace o

-- | The trace's type, if plotly.js knows it. A trace without a @type@ is a
-- scatter trace.
someTraceType :: SomeTrace -> Maybe TraceType
someTraceType (SomeTrace (Fields o)) = case KM.lookup "type" o of
  Nothing -> Just Scatter
  Just (Leaf (String t)) -> traceTypeFromName t
  Just _ -> Nothing

-- | The trace, if it has type @t@.
fromSomeTrace :: forall t. (KnownTraceType t) => SomeTrace -> Maybe (Trace t)
fromSomeTrace tr@(SomeTrace o)
  | someTraceType tr == Just (traceTypeOf (Proxy @t)) = Just (Trace o)
  | otherwise = Nothing

-- | A trace of the named type and no attributes, for a type outside the
-- schema. Set its attributes with 'attr'.
rawTrace :: Text -> SomeTrace
rawTrace t = SomeTrace (Fields (KM.singleton "type" (Leaf (String t))))

-- | An object nested in a trace, the layout, or another object, named by its
-- path from there: @Obj "scatter.marker"@, @Obj "layout.xaxis"@, ….
newtype Obj (path :: Symbol) = Obj Fields
  deriving stock (Eq, Show)

type role Obj nominal

-- | An object with nothing set.
emptyObj :: Obj path
emptyObj = Obj (Fields KM.empty)

instance ToJSON (Obj path) where
  toJSON = Object . toObject

instance FromJSON (Obj path) where
  parseJSON = withObject "Obj" (pure . fromObject)

-- | A figure's layout.
type Layout = Obj "layout"

-- | A figure's configuration, given to @Plotly.newPlot@ but not part of the
-- plot itself.
type Config = Obj "config"

-- | An animation frame.
type Frame = Obj "frame"

-- | Options for @Plotly.animate@.
type Animation = Obj "animation"

-- | An x axis: @layout.xaxis@, @layout.xaxis2@, ….
type XAxis = Obj "layout.xaxis"

-- | A y axis: @layout.yaxis@, @layout.yaxis2@, ….
type YAxis = Obj "layout.yaxis"

-- | A shape in @layout.shapes@.
type Shape = Obj "layout.shape"

-- | An annotation in @layout.annotations@.
type Annotation = Obj "layout.annotation"

-- | A slider in @layout.sliders@.
type Slider = Obj "layout.slider"

-- | A step of a slider.
type SliderStep = Obj "layout.slider.step"

-- | A menu of buttons or a dropdown in @layout.updatemenus@.
type Updatemenu = Obj "layout.updatemenu"

-- | A button of an update menu.
type Button = Obj "layout.updatemenu.button"

-- | A figure: traces, a layout, a configuration and animation frames. Its
-- JSON is plotly.js's figure format, as read by plotly.py's
-- @plotly.io.from_json@ and PlotlyBase.jl. The configuration and frames are
-- left out when empty.
--
-- @g@ is the figure's grid of subplots: 'NoGrid' for a figure made with
-- 'figure', or the grid 'Graphics.Plotly.Subplots.makeSubplots' laid out,
-- so that traces can only be placed in cells that exist and fit them.
data Figure (g :: FigureGrid) = Figure
  { data_ :: !(Seq SomeTrace),
    layout :: !Layout,
    config :: !Config,
    frames :: !(Seq Frame),
    -- | Where the grid's subplots are, if @g@ is a grid.
    figGrid :: !(FigureGridInfo g)
  }
  deriving stock (Eq, Show)

type role Figure nominal

-- | What a figure knows of its grid, by its type: nothing for 'NoGrid',
-- and where the subplots are for a grid.
data FigureGridInfo (g :: FigureGrid) where
  NoGridInfo :: FigureGridInfo 'NoGrid
  HasGridInfo :: GridInfo -> FigureGridInfo ('Grid cells)

deriving stock instance Eq (FigureGridInfo g)

deriving stock instance Show (FigureGridInfo g)

-- | Where a grid figure's subplots are.
gridInfo :: Figure ('Grid g) -> GridInfo
gridInfo f = case figGrid f of HasGridInfo g -> g

-- | A figure with these traces, and nothing else set.
figure :: [SomeTrace] -> Figure 'NoGrid
figure traces = Figure (Seq.fromList traces) emptyObj emptyObj Seq.empty NoGridInfo

-- | The same figure, no longer placing traces by row and column.
forgetGrid :: Figure g -> Figure 'NoGrid
forgetGrid f = f {figGrid = NoGridInfo}

-- | Which corner of the grid is row 1, column 1.
data StartCell = TopLeft | BottomLeft
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A grid of subplots as laid out: where each cell's subplot is.
data GridInfo = GridInfo
  { gridStartCell :: StartCell,
    -- | The cells, a list per row from row 1; 'Nothing' for a blank cell.
    gridCells :: [[Maybe CellInfo]]
  }
  deriving stock (Eq, Show)

-- | A subplot in a grid.
data CellInfo = CellInfo
  { cellRowspan :: Int,
    cellColspan :: Int,
    -- | What a trace refers to, to be drawn here.
    cellRef :: SubplotRef,
    -- | The same, for the secondary y axis of a cell that has one.
    cellSecondaryY :: Maybe SubplotRef
  }
  deriving stock (Eq, Show)

-- | How a trace refers to its subplot.
data SubplotRef
  = -- | By its x and y axes, as @xaxis@ and @yaxis@.
    AxesRef (AxisId 'X) (AxisId 'Y)
  | -- | By the subplot's kind and number, as @scene@, @geo@ or @subplot@:
    -- @NamedRef NamedScene 2@ is @"scene2"@.
    NamedRef NamedKind Int
  | -- | By its own @domain@, the x and y extents of the cell.
    DomainRef (Double, Double) (Double, Double)
  deriving stock (Eq, Show)

-- | A cell of grid @g@ that holds a subplot of kind @k@, on its secondary
-- y axis for @y2@: a row and column, counted from 1. Only the checked
-- constructors in "Graphics.Plotly.Subplots" make one.
newtype CellIx (g :: [[CellSpec]]) (k :: SubplotKind) (y2 :: Bool) = CellIx (Int, Int)
  deriving stock (Eq, Ord, Show)

type role CellIx nominal nominal nominal

-- | The cell at a row and column, counted from 1, if there is one.
gridCellInfo :: Int -> Int -> GridInfo -> Maybe CellInfo
gridCellInfo r c g = do
  row <- gridCells g !? (r - 1)
  join (row !? (c - 1))

-- | What traces in a cell refer to: its subplot, and its secondary y axis
-- if it has one.
cellRefs :: CellInfo -> [SubplotRef]
cellRefs cell = cellRef cell : maybeToList (cellSecondaryY cell)

-- | What traces on a cell's primary or (for 'True') secondary y axis refer
-- to, if it has that axis.
cellRefFor :: Bool -> CellInfo -> Maybe SubplotRef
cellRefFor secondary cell = if secondary then cellSecondaryY cell else Just (cellRef cell)

instance ToJSON (Figure g) where
  toJSON f =
    object $
      ["data" .= data_ f, "layout" .= layout f]
        <> ["config" .= config f | config f /= emptyObj]
        <> ["frames" .= frames f | not (null (frames f))]

-- | Only a figure with no grid: JSON does not say how one was laid out.
instance FromJSON (Figure 'NoGrid) where
  parseJSON = withObject "Figure" $ \o ->
    Figure
      <$> o .:? "data" .!= Seq.empty
      <*> o .:? "layout" .!= emptyObj
      <*> o .:? "config" .!= emptyObj
      <*> o .:? "frames" .!= Seq.empty
      <*> pure NoGridInfo

instance (k ~ A_Lens, a ~ Seq SomeTrace, b ~ Seq SomeTrace) => LabelOptic "data" k (Figure g) (Figure g) a b where
  labelOptic = lens data_ (\f x -> f {data_ = x})

instance (k ~ A_Lens, a ~ Layout, b ~ Layout) => LabelOptic "layout" k (Figure g) (Figure g) a b where
  labelOptic = lens layout (\f x -> f {layout = x})

instance (k ~ A_Lens, a ~ Config, b ~ Config) => LabelOptic "config" k (Figure g) (Figure g) a b where
  labelOptic = lens config (\f x -> f {config = x})

instance (k ~ A_Lens, a ~ Seq Frame, b ~ Seq Frame) => LabelOptic "frames" k (Figure g) (Figure g) a b where
  labelOptic = lens frames (\f x -> f {frames = x})

-- | An object's attributes, as a tree: an attribute that is an object is
-- kept as one, and one that is an array of objects as a 'Seq' of them, so
-- that changing a nested object, or adding to an array of objects at its
-- end, leaves the rest as it is. Each JSON object has one tree, so trees
-- are equal if their JSON is.
newtype Fields = Fields (KM.KeyMap Node)
  deriving stock (Eq, Show)

-- | An attribute's value.
data Node
  = -- | Neither an object nor a non-empty array of objects.
    Leaf Value
  | Child Fields
  | -- | Not empty.
    Children (Seq Fields)
  deriving stock (Eq, Show)

nodeValue :: Node -> Value
nodeValue = \case
  Leaf v -> v
  Child f -> Object (fieldsObject f)
  Children fs -> Array (V.fromListN (Seq.length fs) (map (Object . fieldsObject) (toList fs)))

valueNode :: Value -> Node
valueNode = \case
  Object o -> Child (objectFields o)
  Array v
    | not (V.null v),
      Just os <- traverse asObject v ->
        Children (Seq.fromList (map objectFields (V.toList os)))
  v -> Leaf v
  where
    asObject = \case
      Object o -> Just o
      _ -> Nothing

fieldsObject :: Fields -> Object
fieldsObject (Fields m) = fmap nodeValue m

objectFields :: Object -> Fields
objectFields = Fields . fmap valueNode

-- | Types that are JSON objects underneath.
class IsObject s where
  -- | That the type is a newtype of 'Fields'.
  isFields :: Coercion s Fields

instance IsObject (Trace t) where
  isFields = Coercion

instance IsObject SomeTrace where
  isFields = Coercion

instance IsObject (Obj path) where
  isFields = Coercion

toFields :: forall s. (IsObject s) => s -> Fields
toFields = coerceWith (isFields @s)

fromFields :: forall s. (IsObject s) => Fields -> s
fromFields = coerceWith (sym (isFields @s))

-- | The object as JSON.
toObject :: (IsObject s) => s -> Object
toObject = fieldsObject . toFields

-- | A JSON object, as an object of this type.
fromObject :: (IsObject s) => Object -> s
fromObject = fromFields . objectFields

nodes :: (IsObject s) => s -> KM.KeyMap Node
nodes s = case toFields s of Fields o -> o

fromNodes :: (IsObject s) => KM.KeyMap Node -> s
fromNodes = fromFields . Fields

-- | The keys of the attributes that are set.
attrKeys :: (IsObject s) => s -> [Key]
attrKeys = KM.keys . nodes

-- | Any attribute, by its dotted path (@"marker.line.width"@), untyped.
-- Setting creates the objects on the path; unsetting removes those it
-- leaves empty.
attr :: (IsObject s) => Text -> Lens' s (Maybe Value)
attr path = lens (getPath keys . nodes) (\s v -> fromNodes (setPath keys v (nodes s)))
  where
    keys = map Key.fromText (T.splitOn "." path)
    getPath [] _ = Nothing
    getPath [k] o = nodeValue <$> KM.lookup k o
    getPath (k : ks) o = case KM.lookup k o of
      Just (Child (Fields o')) -> getPath ks o'
      _ -> Nothing
    setPath [] _ o = o
    setPath [k] v o = alter k (valueNode <$> v) o
    setPath (k : ks) v o = case (KM.lookup k o, v) of
      (Just (Child (Fields c)), _) -> putChild (setPath ks v c)
      (_, Nothing) -> o
      (_, Just _) -> putChild (setPath ks v KM.empty)
      where
        putChild c
          | KM.null c = KM.delete k o
          | otherwise = KM.insert k (Child (Fields c)) o

-- | The attribute with this key, if it is set and decodes as an @a@.
field :: (IsObject s, ToJSON a, FromJSON a) => Key -> Lens' s (Maybe a)
field k = lens get put
  where
    get s = case KM.lookup k (nodes s) of
      Just n | Success a <- fromJSON (nodeValue n) -> Just a
      _ -> Nothing
    put s a = fromNodes (alter k (valueNode . toJSON <$> a) (nodes s))

-- | The object with this key; empty if it is unset. An empty object is
-- removed.
nested :: (IsObject s) => Key -> Lens' s (Obj path)
nested k = lens get put
  where
    get s = case KM.lookup k (nodes s) of
      Just (Child c) -> Obj c
      _ -> emptyObj
    put s (Obj c@(Fields o))
      | KM.null o = fromNodes (KM.delete k (nodes s))
      | otherwise = fromNodes (KM.insert k (Child c) (nodes s))

-- | The array of objects with this key; empty if it is unset. Reading and
-- adding to it does not copy it. An object where an array is meant is read
-- as its only element, and elements that are not objects are dropped. An
-- empty array is removed.
objects :: forall s e. (IsObject s, IsObject e) => Key -> Lens' s (Seq e)
objects k = case isFields @e of
  Coercion -> lens get put
    where
      get s = coerce $ case KM.lookup k (nodes s) of
        Just (Children fs) -> fs
        Just (Child f) -> Seq.singleton f
        Just (Leaf (Array v)) -> Seq.fromList [objectFields o | Object o <- V.toList v]
        _ -> Seq.empty
      put s es
        | null es = fromNodes (KM.delete k (nodes s))
        | otherwise = fromNodes (KM.insert k (Children (coerce es)) (nodes s))

alter :: Key -> Maybe Node -> KM.KeyMap Node -> KM.KeyMap Node
alter k = maybe (KM.delete k) (KM.insert k)

-- | The key of a numbered subplot: @subplotKey "xaxis" 1@ is @xaxis@, and
-- @subplotKey "xaxis" 2@ is @xaxis2@.
subplotKey :: Text -> Int -> Key
subplotKey base = Key.fromText . subplotIdText base
