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
    GridInfo (..),
    CellInfo (..),
    SubplotRef (..),
    StartCell (..),
    gridCellInfo,
    cellRefs,
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
    IsObject (..),
    attr,

    -- * Building optics
    field,
    nested,
    subplotKey,
    axisId,
    axisKeyOf,
  )
where

import Control.Monad (join)
import Data.Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeLits (Symbol)
import Graphics.Plotly.Grid (CellSpec, FigureGrid (..))
import Graphics.Plotly.TraceType
import Graphics.Plotly.Types (SubplotId)
import Optics.Core

-- | A trace of type @t@: @Trace Scatter@, @Trace Bar@, ….
newtype Trace (t :: TraceType) = Trace Object
  deriving stock (Eq, Show)

type role Trace nominal

-- | A trace with only its type set. Each trace module exports one under the
-- trace type's name: 'Graphics.Plotly.Trace.Scatter.scatter', ….
newTrace :: forall t. (KnownTraceType t) => Trace t
newTrace = Trace (KM.singleton "type" (String (traceTypeName (traceTypeOf (Proxy @t)))))

instance ToJSON (Trace t) where
  toJSON (Trace o) = Object o

-- | Fails if the @type@ is not @t@.
instance (KnownTraceType t) => FromJSON (Trace t) where
  parseJSON = withObject "Trace" $ \o -> case fromSomeTrace (SomeTrace o) of
    Just tr -> pure tr
    Nothing -> fail ("Trace: not a " <> T.unpack (traceTypeName (traceTypeOf (Proxy @t))) <> " trace")

-- | A trace of any type, as a figure holds them.
newtype SomeTrace = SomeTrace Object
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

-- | Forget a trace's type, to put it in a figure.
someTrace :: Trace t -> SomeTrace
someTrace (Trace o) = SomeTrace o

-- | The trace's type, if plotly.js knows it. A trace without a @type@ is a
-- scatter trace.
someTraceType :: SomeTrace -> Maybe TraceType
someTraceType (SomeTrace o) = case KM.lookup "type" o of
  Nothing -> Just Scatter
  Just (String t) -> traceTypeFromName t
  Just _ -> Nothing

-- | The trace, if it has type @t@.
fromSomeTrace :: forall t. (KnownTraceType t) => SomeTrace -> Maybe (Trace t)
fromSomeTrace tr@(SomeTrace o)
  | someTraceType tr == Just (traceTypeOf (Proxy @t)) = Just (Trace o)
  | otherwise = Nothing

-- | A trace of the named type and no attributes, for a type outside the
-- schema. Set its attributes with 'attr'.
rawTrace :: Text -> SomeTrace
rawTrace t = SomeTrace (KM.singleton "type" (String t))

-- | An object nested in a trace, the layout, or another object, named by its
-- path from there: @Obj "scatter.marker"@, @Obj "layout.xaxis"@, ….
newtype Obj (path :: Symbol) = Obj Object
  deriving stock (Eq, Show)

type role Obj nominal

-- | An object with nothing set.
emptyObj :: Obj path
emptyObj = Obj KM.empty

instance ToJSON (Obj path) where
  toJSON (Obj o) = Object o

instance FromJSON (Obj path) where
  parseJSON = withObject "Obj" (pure . Obj)

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
  { data_ :: [SomeTrace],
    layout :: Layout,
    config :: Config,
    frames :: [Frame],
    -- | Where the grid's subplots are; 'Just' exactly when @g@ is a grid.
    figGrid :: Maybe GridInfo
  }
  deriving stock (Eq, Show)

type role Figure nominal

-- | A figure with these traces, and nothing else set.
figure :: [SomeTrace] -> Figure 'NoGrid
figure traces = Figure traces emptyObj emptyObj [] Nothing

-- | The same figure, no longer placing traces by row and column.
forgetGrid :: Figure g -> Figure 'NoGrid
forgetGrid f = f {figGrid = Nothing}

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
  = -- | By its x and y axes, as @xaxis@ and @yaxis@: @"x2"@ and @"y2"@.
    AxesRef SubplotId SubplotId
  | -- | By the subplot's id, as @scene@, @geo@ or @subplot@: @"scene2"@,
    -- @"polar"@, ….
    NamedRef NamedKind SubplotId
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
  row <- index r (gridCells g)
  join (index c row)
  where
    index i xs = case drop (i - 1) xs of
      x : _ | i >= 1 -> Just x
      _ -> Nothing

-- | What traces in a cell refer to: its subplot, and its secondary y axis
-- if it has one.
cellRefs :: CellInfo -> [SubplotRef]
cellRefs cell = cellRef cell : maybe [] pure (cellSecondaryY cell)

-- | The layout key of a subplot kind's objects, and its prefix in ids.
subplotName :: NamedKind -> Text
subplotName = \case
  NamedScene -> "scene"
  NamedGeo -> "geo"
  NamedPolar -> "polar"
  NamedTernary -> "ternary"
  NamedMap -> "map"
  NamedSmith -> "smith"

-- | The trace attribute that names a subplot of this kind.
subplotAttr :: NamedKind -> Text
subplotAttr = \case
  NamedScene -> "scene"
  NamedGeo -> "geo"
  _ -> "subplot"

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
      <$> o .:? "data" .!= []
      <*> o .:? "layout" .!= emptyObj
      <*> o .:? "config" .!= emptyObj
      <*> o .:? "frames" .!= []
      <*> pure Nothing

instance (k ~ A_Lens, a ~ [SomeTrace], b ~ [SomeTrace]) => LabelOptic "data" k (Figure g) (Figure g) a b where
  labelOptic = lens data_ (\f x -> f {data_ = x})

instance (k ~ A_Lens, a ~ Layout, b ~ Layout) => LabelOptic "layout" k (Figure g) (Figure g) a b where
  labelOptic = lens layout (\f x -> f {layout = x})

instance (k ~ A_Lens, a ~ Config, b ~ Config) => LabelOptic "config" k (Figure g) (Figure g) a b where
  labelOptic = lens config (\f x -> f {config = x})

instance (k ~ A_Lens, a ~ [Frame], b ~ [Frame]) => LabelOptic "frames" k (Figure g) (Figure g) a b where
  labelOptic = lens frames (\f x -> f {frames = x})

-- | Types that are JSON objects underneath.
class IsObject s where
  toObject :: s -> Object
  fromObject :: Object -> s

instance IsObject (Trace t) where
  toObject (Trace o) = o
  fromObject = Trace

instance IsObject SomeTrace where
  toObject (SomeTrace o) = o
  fromObject = SomeTrace

instance IsObject (Obj path) where
  toObject (Obj o) = o
  fromObject = Obj

-- | Any attribute, by its dotted path (@"marker.line.width"@), untyped.
-- Setting creates the objects on the path; unsetting removes those it
-- leaves empty.
attr :: (IsObject s) => Text -> Lens' s (Maybe Value)
attr path = lens (getPath keys . toObject) (\s v -> fromObject (setPath keys v (toObject s)))
  where
    keys = map Key.fromText (T.splitOn "." path)
    getPath [] _ = Nothing
    getPath [k] o = KM.lookup k o
    getPath (k : ks) o = case KM.lookup k o of
      Just (Object o') -> getPath ks o'
      _ -> Nothing
    setPath [] _ o = o
    setPath [k] v o = alter k v o
    setPath (k : ks) v o = case (KM.lookup k o, v) of
      (Just (Object c), _) -> putChild (setPath ks v c)
      (_, Nothing) -> o
      (_, Just _) -> putChild (setPath ks v KM.empty)
      where
        putChild c
          | KM.null c = KM.delete k o
          | otherwise = KM.insert k (Object c) o

-- | The attribute with this key, if it is set and decodes as an @a@.
field :: (IsObject s, ToJSON a, FromJSON a) => Key -> Lens' s (Maybe a)
field k = lens get put
  where
    get s = case KM.lookup k (toObject s) of
      Just v | Success a <- fromJSON v -> Just a
      _ -> Nothing
    put s a = fromObject (alter k (toJSON <$> a) (toObject s))

-- | The object with this key; empty if it is unset. An empty object is
-- removed.
nested :: (IsObject s) => Key -> Lens' s (Obj path)
nested k = lens get put
  where
    get s = case KM.lookup k (toObject s) of
      Just (Object o) -> Obj o
      _ -> emptyObj
    put s (Obj o)
      | KM.null o = fromObject (KM.delete k (toObject s))
      | otherwise = fromObject (KM.insert k (Object o) (toObject s))

alter :: Key -> Maybe Value -> Object -> Object
alter k = maybe (KM.delete k) (KM.insert k)

-- | The key of a numbered subplot: @subplotKey "xaxis" 1@ is @xaxis@, and
-- @subplotKey "xaxis" 2@ is @xaxis2@.
subplotKey :: Text -> Int -> Key
subplotKey base = Key.fromText . axisId base

-- | The id of a numbered subplot or axis: @axisId "x" 1@ is @"x"@ and
-- @axisId "x" 2@ is @"x2"@.
axisId :: Text -> Int -> Text
axisId base n
  | n <= 1 = base
  | otherwise = base <> T.pack (show n)

-- | The layout key of an axis id: @"x2"@ is @xaxis2@.
axisKeyOf :: Text -> Key
axisKeyOf ax = Key.fromText (T.take 1 ax <> "axis" <> T.drop 1 ax)
