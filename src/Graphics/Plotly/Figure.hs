{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Working with a whole figure, as plotly.py's @Figure@ methods do: adding
-- and updating traces, updating axes, adding shapes and annotations, and
-- animating. Where plotly.py takes @row@ and @col@, these take 'Cells' of
-- the figure's subplot grid (see "Graphics.Plotly.Subplots").
--
-- @
-- fig
--   & updateTracesOf \@Scatter mempty (#marker % #size ?~ One 10)
--   & updateXAxes (inRow 2) (#showgrid ?~ False)
--   & addHLine defaultSpanOptions (0 :: Double)
-- @
module Graphics.Plotly.Figure
  ( -- * Traces
    addTrace,
    addTraces,

    -- ** Selecting traces
    Selector,
    ofType,
    atIndex,
    named,
    inCells,
    onSecondaryY,
    where_,
    withAttr,
    traces,
    tracesOf,
    selectTraces,
    updateTraces,
    updateTracesOf,
    forEachTrace,

    -- * Cells of a grid
    Cells,
    allCells,
    inRow,
    inCol,
    atCell,
    cellsOf,
    primaryY,
    secondaryY,

    -- * Layout and axes
    updateLayout,
    xaxes,
    yaxes,
    updateXAxes,
    updateYAxes,

    -- * Shapes and annotations
    addShape,
    addAnnotation,
    addShapeAt,
    addAnnotationAt,

    -- ** Lines and rectangles across a subplot
    addHLine,
    addVLine,
    addHRect,
    addVRect,
    SpanOptions (..),
    defaultSpanOptions,
    AnnotationPosition (..),

    -- * Animation
    frame,
    addFrames,
    animationButton,
    playPauseMenu,
    framesSlider,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson (Result (..), ToJSON, Value (..), fromJSON, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isDigit)
import Data.Foldable (asum)
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, isJust, mapMaybe, maybeToList)
import Data.Proxy (Proxy (..))
import Data.Scientific (toRealFloat)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (LocalTime (..), addLocalTime, diffLocalTime, midnight, utc, utcToLocalTime, zonedTimeToUTC)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import GHC.TypeLits (KnownNat)
import Graphics.Plotly.Animation ()
import Graphics.Plotly.Enums (AnimationMode (..), AnnotationXanchor (..), AnnotationXref (..), AnnotationYanchor (..), Method (..), ShapeType (..), UpdatemenuDirection (..), UpdatemenuType (..))
import Graphics.Plotly.Frame ()
import Graphics.Plotly.Grid (CheckCell, CheckCell', CheckCol, CheckCol', CheckRow, CheckRow', check, natInt)
import Graphics.Plotly.Internal.Core
import Graphics.Plotly.Layout (HasAxisRefs (..))
import Graphics.Plotly.Subplots (KnownY2 (..), cellPosition)
import Graphics.Plotly.Types (ArrayOk (..), SubplotId (..))
import Optics.Core

-- * Traces

-- | Add a trace at the end.
addTrace :: Trace t -> Figure g -> Figure g
addTrace tr = #data %~ (<> [someTrace tr])

-- | Add traces at the end.
addTraces :: [SomeTrace] -> Figure g -> Figure g
addTraces trs = #data %~ (<> trs)

-- | Which of a figure's traces to work on. @a <> b@ selects the traces
-- that both select, and 'mempty' selects them all.
newtype Selector (g :: FigureGrid) = Selector (FigureGridInfo g -> Int -> SomeTrace -> Bool)

instance Semigroup (Selector g) where
  Selector a <> Selector b = Selector (\g -> let a' = a g; b' = b g in \i t -> a' i t && b' i t)

instance Monoid (Selector g) where
  mempty = Selector (\_ _ _ -> True)

-- | Traces of type @t@: @ofType \@Bar@.
ofType :: forall t g. (KnownTraceType t) => Selector g
ofType = Selector (\_ _ t -> someTraceType t == Just (traceTypeOf (Proxy @t)))

-- | The trace at this position in the figure's list, from 0.
atIndex :: Int -> Selector g
atIndex n = Selector (\_ i _ -> i == n)

-- | Traces with this @name@.
named :: Text -> Selector g
named n = withAttr "name" (String n)

-- | Traces in these cells of the figure's grid, on the y axes they select.
inCells :: Cells ('Grid g) -> Selector ('Grid g)
inCells cs = onRefs (`selectRefs` cs)

-- | Traces on a secondary y axis of the figure's grid.
onSecondaryY :: Selector ('Grid g)
onSecondaryY = inCells (secondaryY allCells)

-- | Traces on one of these subplots of the figure's grid.
onRefs :: (GridInfo -> [SubplotRef]) -> Selector ('Grid g)
onRefs refsOf = Selector $ \(HasGridInfo g) ->
  let refs = refsOf g
   in \_ t -> maybe False (`elem` refs) (traceRef t)

-- | Traces for which the function is 'True'.
where_ :: (SomeTrace -> Bool) -> Selector g
where_ p = Selector (\_ _ t -> p t)

-- | Traces whose attribute at the dotted path has this value, as with
-- plotly.py's dictionary selectors.
withAttr :: Text -> Value -> Selector g
withAttr path v = where_ (\t -> t ^. attr path == Just v)

-- | The selected traces.
traces :: Selector g -> Traversal' (Figure g) SomeTrace
traces (Selector p) = traversalVL $ \f fig ->
  let p' = p (figGrid fig)
      go (i, t)
        | p' i t = f t
        | otherwise = pure t
   in (\ts -> fig {data_ = ts}) <$> traverse go (zip [0 ..] (data_ fig))

-- | The selected traces of type @t@, typed: @tracesOf \@Scatter mempty@.
tracesOf :: forall t g. (KnownTraceType t) => Selector g -> Traversal' (Figure g) (Trace t)
tracesOf sel = traces (ofType @t <> sel) % traversalVL (\f t -> maybe (pure t) (fmap someTrace . f) (fromSomeTrace t))

-- | The selected traces, in order.
selectTraces :: Selector g -> Figure g -> [SomeTrace]
selectTraces sel = toListOf (traces sel)

-- | Update the selected traces.
updateTraces :: Selector g -> (SomeTrace -> SomeTrace) -> Figure g -> Figure g
updateTraces sel = over (traces sel)

-- | Update the selected traces of type @t@, with their typed optics:
-- @updateTracesOf \@Bar mempty (#opacity ?~ 0.5)@.
updateTracesOf :: forall t g. (KnownTraceType t) => Selector g -> (Trace t -> Trace t) -> Figure g -> Figure g
updateTracesOf sel = over (tracesOf @t sel)

-- | Run an action on each selected trace, in order, and replace it with
-- the result.
forEachTrace :: (Applicative f) => Selector g -> (SomeTrace -> f SomeTrace) -> Figure g -> f (Figure g)
forEachTrace sel = traverseOf (traces sel)

-- * Cells

-- | Cells of the subplot grid of a @'Figure' g@, by row and column,
-- counted from 1. Rows and columns are checked against the grid at compile
-- time. They select either y axis of a cell with a secondary one, unless
-- narrowed with 'primaryY' or 'secondaryY'.
data Cells (g :: FigureGrid) = Cells (Maybe Int) (Maybe Int) WhichY
  deriving stock (Eq, Show)

type role Cells nominal

-- | Which y axes of a cell are selected, as plotly.py's @secondary_y@.
data WhichY = EitherY | PrimaryY | SecondaryY
  deriving stock (Eq, Show)

-- | Every cell; for a figure with no grid, the whole figure.
allCells :: Cells g
allCells = Cells Nothing Nothing EitherY

-- | The cells in row @r@: @inRow \@2@.
inRow :: forall r g. (KnownNat r, CheckRow g r) => Cells ('Grid g)
inRow = check @(CheckRow' g r) (Cells (Just (natInt @r)) Nothing EitherY)

-- | The cells in column @c@.
inCol :: forall c g. (KnownNat c, CheckCol g c) => Cells ('Grid g)
inCol = check @(CheckCol' g c) (Cells Nothing (Just (natInt @c)) EitherY)

-- | The cell at row @r@ and column @c@, which must hold a subplot.
atCell :: forall r c g. (KnownNat r, KnownNat c, CheckCell g r c) => Cells ('Grid g)
atCell = check @(CheckCell' g r c) (Cells (Just (natInt @r)) (Just (natInt @c)) EitherY)

-- | The subplot a cell index names: its secondary y axis for a 'cellY2'
-- index, and its primary one otherwise, as 'addTraceIn' places a trace.
cellsOf :: forall g k y2. (KnownY2 y2) => CellIx g k y2 -> Cells ('Grid g)
cellsOf i =
  let (r, c) = cellPosition i
   in Cells (Just r) (Just c) (if y2Val @y2 then SecondaryY else PrimaryY)

-- | Only the primary y axes of these cells.
primaryY :: Cells ('Grid g) -> Cells ('Grid g)
primaryY (Cells r c _) = Cells r c PrimaryY

-- | Only the secondary y axes of these cells; cells without one are
-- skipped.
secondaryY :: Cells ('Grid g) -> Cells ('Grid g)
secondaryY (Cells r c _) = Cells r c SecondaryY

-- | What traces on the selected y axes of the selected cells refer to, by
-- row.
selectRefs :: GridInfo -> Cells g -> [SubplotRef]
selectRefs g (Cells mr mc which) =
  [ ref
  | (r, row) <- zip [1 ..] (gridCells g),
    maybe True (== r) mr,
    (c, Just cell) <- zip [1 ..] row,
    maybe True (== c) mc,
    ref <- case which of
      EitherY -> cellRefs cell
      PrimaryY -> [cellRef cell]
      SecondaryY -> maybeToList (cellSecondaryY cell)
  ]

-- | The subplot a trace is on, as a cell would refer to it.
traceRef :: SomeTrace -> Maybe SubplotRef
traceRef t =
  someTraceType t >>= traceSubplotKind >>= \case
    SubplotXY -> Just (AxesRef (SubplotId (text "xaxis" "x")) (SubplotId (text "yaxis" "y")))
    SubplotDomain -> DomainRef <$> pair "domain.x" <*> pair "domain.y"
    SubplotNamed k -> Just (NamedRef k (SubplotId (text (subplotAttr k) (subplotName k))))
  where
    text k d = textAttr k d t
    pair k = case fromJSON <$> t ^. attr k of
      Just (Success p) -> Just p
      _ -> Nothing

-- | A text attribute at the dotted path, or the default if it is unset or
-- not text.
textAttr :: (IsObject s) => Text -> Text -> s -> Text
textAttr k d o = case o ^. attr k of
  Just (String s) -> s
  _ -> d

-- * Layout and axes

-- | Update the layout.
updateLayout :: (Layout -> Layout) -> Figure g -> Figure g
updateLayout = over #layout

-- | The x axes of the selected cells. With 'allCells', or in a figure with
-- no grid, every x axis in the layout, or @xaxis@ if there are none.
xaxes :: Cells g -> Traversal' (Figure g) XAxis
xaxes = axes "x" (\case AxesRef (SubplotId x) _ -> [x]; _ -> [])

-- | The y axes of the selected cells, secondary ones included. With
-- 'allCells', or in a figure with no grid, every y axis in the layout, or
-- @yaxis@ if there are none.
yaxes :: Cells g -> Traversal' (Figure g) YAxis
yaxes = axes "y" (\case AxesRef _ (SubplotId y) -> [y]; _ -> [])

axes :: Text -> (SubplotRef -> [Text]) -> Cells g -> Traversal' (Figure g) (Obj path)
axes xy fromRef cs = traversalVL $ \f fig ->
  let lay = layout fig
      inLayout = [k | k <- KM.keys (toObject lay), isJust (axisNumber k)]
      ks = case figGrid fig of
        HasGridInfo g
          | cs /= allCells ->
              nub [axisKeyOf a | ref <- selectRefs g cs, a <- fromRef ref]
        _
          | null inLayout -> [Key.fromText (xy <> "axis")]
          | otherwise -> sortOn axisNumber inLayout
      -- Each axis is read from the original layout and written in turn.
      layout' = foldl (\acc k -> (\l v -> l & nested k .~ v) <$> acc <*> f (lay ^. nested k)) (pure lay) ks
   in (\l -> fig {layout = l}) <$> layout'
  where
    -- The number of an axis's key: 1 for @xaxis@, 2 for @xaxis2@.
    axisNumber k = case T.stripPrefix (xy <> "axis") (Key.toText k) of
      Just "" -> Just (1 :: Int)
      Just n | T.all isDigit n -> Just (read (T.unpack n))
      _ -> Nothing

-- | Update the x axes of the selected cells (see 'xaxes').
updateXAxes :: Cells g -> (XAxis -> XAxis) -> Figure g -> Figure g
updateXAxes cs = over (xaxes cs)

-- | Update the y axes of the selected cells (see 'yaxes').
updateYAxes :: Cells g -> (YAxis -> YAxis) -> Figure g -> Figure g
updateYAxes cs = over (yaxes cs)

-- * Shapes and annotations

-- | Add a shape at the end of @layout.shapes@.
addShape :: Shape -> Figure g -> Figure g
addShape = addShapes . pure

-- | Add an annotation at the end of @layout.annotations@.
addAnnotation :: Annotation -> Figure g -> Figure g
addAnnotation = addAnnotations . pure

addShapes :: [Shape] -> Figure g -> Figure g
addShapes ss = #layout % #shapes % non [] %~ (<> ss)

addAnnotations :: [Annotation] -> Figure g -> Figure g
addAnnotations as = #layout % #annotations % non [] %~ (<> as)

-- | Add a shape to each selected cartesian cell of the figure's grid,
-- referring to its axes (@xref@ and @yref@). A reference ending in
-- @domain@ keeps doing so: @"x domain"@ becomes @"x2 domain"@. A @yref@ set
-- to another axis is kept. Other cells are skipped, and in a figure with no
-- grid the shape is added once, as it is.
addShapeAt :: Cells g -> Shape -> Figure g -> Figure g
addShapeAt cs s fig = addShapes (placeAt cs s fig) fig

-- | Add an annotation to each selected cartesian cell, as 'addShapeAt'
-- adds a shape.
addAnnotationAt :: Cells g -> Annotation -> Figure g -> Figure g
addAnnotationAt cs a fig = addAnnotations (placeAt cs a fig) fig

-- | Copies of a shape or annotation, one per selected cartesian cell.
placeAt :: (AxisPlaced s) => Cells g -> s -> Figure g -> [s]
placeAt cs s fig = case figGrid fig of
  NoGridInfo -> [s]
  HasGridInfo g -> [withRefs x y s | (x, y) <- cellAxes g cs]

-- | The x and y axes of the selected cartesian cells. Something placed on
-- a cell goes on its primary y axis unless 'secondaryY' selects only that
-- one, as plotly.py's @secondary_y=None@ places it.
cellAxes :: GridInfo -> Cells g -> [(Text, Text)]
cellAxes g (Cells r c which) = [(x, y) | AxesRef (SubplotId x) (SubplotId y) <- selectRefs g (Cells r c which')]
  where
    which' = if which == SecondaryY then SecondaryY else PrimaryY

-- | An @xref@ or @yref@, which may be a single reference.
class SingleRef r where
  -- | The reference, if it is a single one (a shape's may be a list).
  singleRef :: Prism' r AnnotationXref

instance SingleRef AnnotationXref where
  singleRef = castOptic simple

instance SingleRef (ArrayOk AnnotationXref) where
  singleRef = prism' One (\case One r -> Just r; Many _ -> Nothing)

-- | A layout object placed on axes, by single references.
type AxisPlaced s = (HasAxisRefs s, SingleRef (AxisRefOf s))

-- | A single @xref@ or @yref@.
xref1, yref1 :: (AxisPlaced s) => AffineTraversal' s AnnotationXref
xref1 = xrefOf % _Just % singleRef
yref1 = yrefOf % _Just % singleRef

-- | A reference to an axis by its id: @"x2"@, or @"x2 domain"@ for its
-- whole length.
axisRef :: (SingleRef r) => Text -> r
axisRef = review singleRef . AnnotationXrefSubplot . SubplotId

-- | A reference as plotly.js writes it: @"paper"@, @"x2"@, @"x2 domain"@.
refText :: AnnotationXref -> Text
refText = \case
  AnnotationXrefPaper -> "paper"
  AnnotationXrefSubplot (SubplotId r) -> r

-- | Set @xref@ and @yref@ to these axes, keeping @domain@ suffixes, and
-- keeping a @yref@ to some other axis.
withRefs :: (AxisPlaced s) => Text -> Text -> s -> s
withRefs x y s =
  s
    & xrefOf ?~ axisRef (suffixed xr x)
    & (if ownY then yrefOf ?~ axisRef (suffixed yr y) else id)
  where
    xr = refText <$> s ^? xref1
    yr = refText <$> s ^? yref1
    ownY = case yr of
      Nothing -> True
      Just r -> r == "y" || "paper" `T.isInfixOf` r || "domain" `T.isInfixOf` r
    suffixed old new
      | maybe False ("domain" `T.isInfixOf`) old = new <> " domain"
      | otherwise = new

-- | Options for 'addHLine', 'addVLine', 'addHRect' and 'addVRect'.
data SpanOptions (g :: FigureGrid) = SpanOptions
  { -- | The cells to draw in. In a figure with no grid, the shape is drawn
    -- once, on the @x@ and @y@ axes.
    cells :: Cells g,
    -- | Skip cells with nothing in them: no traces, shapes, annotations or
    -- images on their axes.
    excludeEmptySubplots :: Bool,
    -- | Set the shape's attributes: @#line % #dash ?~ "dot"@, ….
    shapeStyle :: Shape -> Shape,
    -- | An annotation to go with each shape, if any:
    -- @Just (#text ?~ "limit")@. Its position is set from
    -- 'annotationPosition' unless the function sets it.
    annotation :: Maybe (Annotation -> Annotation),
    -- | Where the annotation goes on the line or rectangle.
    annotationPosition :: AnnotationPosition,
    -- | Put a rectangle's annotation outside it, rather than inside.
    annotationOutside :: Bool
  }

-- | All cells, skipping empty ones, with no styling or annotation (which
-- would go at the top right, inside a rectangle).
defaultSpanOptions :: SpanOptions g
defaultSpanOptions = SpanOptions allCells True id Nothing AtTopRight False

-- | A horizontal line at this y, across the whole width of each subplot.
addHLine :: (ToJSON a) => SpanOptions g -> a -> Figure g -> Figure g
addHLine opts y = spanning HLine opts (Number 0, Number 1, toJSON y, toJSON y)

-- | A vertical line at this x, across the whole height of each subplot.
addVLine :: (ToJSON a) => SpanOptions g -> a -> Figure g -> Figure g
addVLine opts x = spanning VLine opts (toJSON x, toJSON x, Number 0, Number 1)

-- | A rectangle between these y values, across the whole width of each
-- subplot.
addHRect :: (ToJSON a) => SpanOptions g -> a -> a -> Figure g -> Figure g
addHRect opts y0 y1 = spanning HRect opts (Number 0, Number 1, toJSON y0, toJSON y1)

-- | A rectangle between these x values, across the whole height of each
-- subplot.
addVRect :: (ToJSON a) => SpanOptions g -> a -> a -> Figure g -> Figure g
addVRect opts x0 x1 = spanning VRect opts (toJSON x0, toJSON x1, Number 0, Number 1)

data Spanning = HLine | VLine | HRect | VRect
  deriving stock (Eq)

-- | Add an axis-spanning shape, and its annotation, as plotly.py's
-- @_process_multiple_axis_spanning_shapes@ does.
spanning :: Spanning -> SpanOptions g -> (Value, Value, Value, Value) -> Figure g -> Figure g
spanning kind opts (x0, x1, y0, y1) fig =
  addAnnotations annotations (addShapes shapes fig)
  where
    horizontal = kind `elem` [HLine, HRect]
    base :: Shape
    base =
      emptyObj
        & #type ?~ (if kind `elem` [HLine, VLine] then ShapeTypeLine else ShapeTypeRect)
        & #x0 ?~ x0
        & #x1 ?~ x1
        & #y0 ?~ y0
        & #y1 ?~ y1
    styled = shapeStyle opts base
    -- A copy in each target cell, on its axes.
    placed :: (AxisPlaced s) => s -> [s]
    placed s = map toDomain $ case figGrid fig of
      NoGridInfo -> [withDefaultRefs s]
      HasGridInfo g ->
        [ s'
        | (x, y) <- cellAxes g (cells opts),
          let s' = withRefs x y s
              -- The axes it is drawn on, as plotly.py checks them: a yref
              -- to another axis is kept.
              y' = maybe y ((\r -> fromMaybe r (T.stripSuffix " domain" r)) . refText) (s' ^? yref1),
          not (excludeEmptySubplots opts) || (x, y') `Set.member` occupied
        ]
    shapes = placed styled
    -- The annotation goes on the shape's y axis, as plotly.py puts it.
    annotations = case annotation opts of
      Nothing -> []
      Just f -> placed (positioned (f emptyObj) & yrefOf .~ (review singleRef <$> styled ^? yref1))
    -- The anchors and position, where the function did not set them.
    positioned :: Annotation -> Annotation
    positioned a =
      a
        & #xanchor %~ (<|> Just xa)
        & #yanchor %~ (<|> Just ya)
        & #x %~ orValue ax
        & #y %~ orValue ay
        & #showarrow %~ (<|> Just False)
      where
        Anchored xa ya ax ay = annotationParams kind (annotationPosition opts) (annotationOutside opts) (x0, x1, y0, y1)
        orValue v = \case
          Nothing -> Just v
          Just Null -> Just v
          old -> old
    withDefaultRefs :: (AxisPlaced s) => s -> s
    withDefaultRefs s =
      s
        & xrefOf %~ Just . fromMaybe (axisRef "x")
        & yrefOf %~ Just . fromMaybe (axisRef "y")
    toDomain :: (AxisPlaced s) => s -> s
    toDomain =
      (if horizontal then xref1 else yref1) %~ \case
        AnnotationXrefSubplot (SubplotId r)
          | not ("domain" `T.isInfixOf` r) -> AnnotationXrefSubplot (SubplotId (r <> " domain"))
        r -> r
    -- The axes that something is drawn on. Only cartesian traces are on
    -- axes: a pie is not on x and y.
    occupied =
      Set.fromList $
        [(x, y) | Just (AxesRef (SubplotId x) (SubplotId y)) <- map traceRef (data_ fig)]
          <> map refAxes (layout fig ^. #shapes % non [])
          <> map refAxes (layout fig ^. #annotations % non [])
          <> map refAxes (layout fig ^. #images % non [])
    refAxes :: (AxisPlaced s) => s -> (Text, Text)
    refAxes o = (maybe "x" refText (o ^? xref1), maybe "y" refText (o ^? yref1))

-- | Where an annotation goes on its line or rectangle.
data AnnotationPosition
  = AtTop
  | AtBottom
  | AtLeft
  | AtRight
  | AtTopLeft
  | AtTopRight
  | AtBottomLeft
  | AtBottomRight
  | -- | The middle of the line or rectangle.
    AtCenter
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | An annotation's anchors, @x@ and @y@.
data Anchored = Anchored AnnotationXanchor AnnotationYanchor Value Value

-- | The annotation's anchors and position for a position on a line or
-- rectangle, as plotly.py's @shapeannotation@ computes them. A rectangle's
-- annotation is outside it for 'True'.
annotationParams :: Spanning -> AnnotationPosition -> Bool -> (Value, Value, Value, Value) -> Anchored
annotationParams kind position outside (x0, x1, y0, y1) = case kind of
  VLine -> case position of
    AtTopLeft -> anno AnnotationXanchorRight AnnotationYanchorTop (pick (argmax ys) xs) (maxV ys)
    AtTopRight -> anno AnnotationXanchorLeft AnnotationYanchorTop (pick (argmax ys) xs) (maxV ys)
    AtTop -> anno AnnotationXanchorCenter AnnotationYanchorBottom (pick (argmax ys) xs) (maxV ys)
    AtBottomLeft -> anno AnnotationXanchorRight AnnotationYanchorBottom (pick (argmin ys) xs) (minV ys)
    AtBottomRight -> anno AnnotationXanchorLeft AnnotationYanchorBottom (pick (argmin ys) xs) (minV ys)
    AtBottom -> anno AnnotationXanchorCenter AnnotationYanchorTop (pick (argmin ys) xs) (minV ys)
    AtLeft -> anno AnnotationXanchorRight AnnotationYanchorMiddle (meanV xs) (meanV ys)
    AtRight -> anno AnnotationXanchorLeft AnnotationYanchorMiddle (meanV xs) (meanV ys)
    AtCenter -> centre
  HLine -> case position of
    AtTopLeft -> anno AnnotationXanchorLeft AnnotationYanchorBottom (minV xs) (pick (argmin xs) ys)
    AtTopRight -> anno AnnotationXanchorRight AnnotationYanchorBottom (maxV xs) (pick (argmax xs) ys)
    AtTop -> anno AnnotationXanchorCenter AnnotationYanchorBottom (meanV xs) (meanV ys)
    AtBottomLeft -> anno AnnotationXanchorLeft AnnotationYanchorTop (minV xs) (pick (argmin xs) ys)
    AtBottomRight -> anno AnnotationXanchorRight AnnotationYanchorTop (maxV xs) (pick (argmax xs) ys)
    AtBottom -> anno AnnotationXanchorCenter AnnotationYanchorTop (meanV xs) (meanV ys)
    AtLeft -> anno AnnotationXanchorRight AnnotationYanchorMiddle (minV xs) (pick (argmin xs) ys)
    AtRight -> anno AnnotationXanchorLeft AnnotationYanchorMiddle (maxV xs) (pick (argmax xs) ys)
    AtCenter -> centre
  _ | not outside -> case position of
    AtTopLeft -> anno AnnotationXanchorLeft AnnotationYanchorTop (minV xs) (maxV ys)
    AtTopRight -> anno AnnotationXanchorRight AnnotationYanchorTop (maxV xs) (maxV ys)
    AtTop -> anno AnnotationXanchorCenter AnnotationYanchorTop (meanV xs) (maxV ys)
    AtBottomLeft -> anno AnnotationXanchorLeft AnnotationYanchorBottom (minV xs) (minV ys)
    AtBottomRight -> anno AnnotationXanchorRight AnnotationYanchorBottom (maxV xs) (minV ys)
    AtBottom -> anno AnnotationXanchorCenter AnnotationYanchorBottom (meanV xs) (minV ys)
    AtLeft -> anno AnnotationXanchorLeft AnnotationYanchorMiddle (minV xs) (meanV ys)
    AtRight -> anno AnnotationXanchorRight AnnotationYanchorMiddle (maxV xs) (meanV ys)
    AtCenter -> centre
  _ -> case position of
    AtTopLeft -> anno (if v then AnnotationXanchorRight else AnnotationXanchorLeft) (if h then AnnotationYanchorBottom else AnnotationYanchorTop) (minV xs) (maxV ys)
    AtTopRight -> anno (if v then AnnotationXanchorLeft else AnnotationXanchorRight) (if h then AnnotationYanchorBottom else AnnotationYanchorTop) (maxV xs) (maxV ys)
    AtTop -> anno AnnotationXanchorCenter AnnotationYanchorBottom (meanV xs) (maxV ys)
    AtBottomLeft -> anno (if v then AnnotationXanchorRight else AnnotationXanchorLeft) (if h then AnnotationYanchorTop else AnnotationYanchorBottom) (minV xs) (minV ys)
    AtBottomRight -> anno (if v then AnnotationXanchorLeft else AnnotationXanchorRight) (if h then AnnotationYanchorTop else AnnotationYanchorBottom) (maxV xs) (minV ys)
    AtBottom -> anno AnnotationXanchorCenter AnnotationYanchorTop (meanV xs) (minV ys)
    AtLeft -> anno AnnotationXanchorRight AnnotationYanchorMiddle (minV xs) (meanV ys)
    AtRight -> anno AnnotationXanchorLeft AnnotationYanchorMiddle (maxV xs) (meanV ys)
    AtCenter -> centre
  where
    anno = Anchored
    centre = anno AnnotationXanchorCenter AnnotationYanchorMiddle (meanV xs) (meanV ys)
    xs = (x0, x1)
    ys = (y0, y1)
    v = kind == VRect
    h = kind == HRect

-- | Coordinates compare as numbers, or else as text (dates in ISO form
-- compare correctly as text).
compareV :: Value -> Value -> Ordering
compareV (Number a) (Number b) = compare a b
compareV (String a) (String b) = compare a b
compareV (Number _) _ = LT
compareV _ (Number _) = GT
compareV _ _ = EQ

-- | Which of two coordinates is the largest: the first, if they are equal,
-- as plotly.py's @_argmax@ picks.
argmax :: (Value, Value) -> Bool
argmax (a, b) = compareV b a /= GT

-- | Which of two coordinates is the smallest: the first, if they are equal.
argmin :: (Value, Value) -> Bool
argmin (a, b) = compareV b a /= LT

-- | The first of a pair for 'True', the second for 'False'.
pick :: Bool -> (a, a) -> a
pick first (a, b) = if first then a else b

maxV :: (Value, Value) -> Value
maxV vs = pick (argmax vs) vs

minV :: (Value, Value) -> Value
minV vs = pick (argmin vs) vs

-- | The middle of two coordinates: numbers, or dates in ISO form
-- (@YYYY-MM-DD@, @YYYY-MM-DD HH:MM@, @YYYY-MM-DD HH:MM:SS@, with a @T@ for
-- the space, fractional seconds or a time zone as you like), the result in
-- UTC as @YYYY-MM-DD HH:MM:SS@. Anything else, such as two categories, has
-- no middle; the first is used.
meanV :: (Value, Value) -> Value
meanV (a, b)
  | a == b = a
  | Number m <- a, Number n <- b = toJSON ((toRealFloat m + toRealFloat n) / 2 :: Double)
  | Just s <- date a,
    Just t <- date b =
      String (T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (addLocalTime (diffLocalTime t s / 2) s)))
  | otherwise = a
  where
    -- A date or time, in UTC if it has a time zone, as plotly.py reads it.
    date = \case
      String str ->
        let str' = T.unpack (T.replace " " "T" (T.strip str))
         in asum
              [ iso8601ParseM str',
                utcToLocalTime utc . zonedTimeToUTC <$> iso8601ParseM str',
                utcToLocalTime utc <$> iso8601ParseM str',
                parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M" str',
                (`LocalTime` midnight) <$> iso8601ParseM str'
              ]
      _ -> Nothing

-- * Animation

-- | A frame with a name and traces. 'framesSlider' and 'animationButton'
-- refer to frames by name.
frame :: Text -> [SomeTrace] -> Frame
frame n trs = emptyObj & #name ?~ n & #data ?~ trs

-- | Add frames at the end.
addFrames :: [Frame] -> Figure g -> Figure g
addFrames fs = #frames %~ (<> fs)

-- | A button that animates through the named frames, or all of them for
-- 'Nothing', with these options.
animationButton :: Text -> Maybe [Text] -> Animation -> Button
animationButton label names anim =
  emptyObj
    & #label ?~ label
    & #method ?~ MethodAnimate
    & #args ?~ [maybe Null toJSON names, toJSON anim]

-- | Play and Pause buttons, placed below the plot on the left as in
-- plotly.py's examples. Play animates through every frame with these
-- options, from the current one; Pause stops at once.
playPauseMenu :: Animation -> Updatemenu
playPauseMenu anim =
  emptyObj
    & #type ?~ UpdatemenuTypeButtons
    & #buttons ?~ [play, pause]
    & #direction ?~ UpdatemenuDirectionLeft
    & #showactive ?~ False
    & #x ?~ 0.1
    & #y ?~ 0
    & #xanchor ?~ AnnotationXanchorRight
    & #yanchor ?~ AnnotationYanchorTop
    & #pad % #r ?~ 10
    & #pad % #t ?~ 87
  where
    play = animationButton "Play" Nothing (anim & #fromcurrent ?~ True)
    pause =
      emptyObj
        & #label ?~ "Pause"
        & #method ?~ MethodAnimate
        & #args ?~ [toJSON [Null], toJSON stop]
    stop :: Animation
    stop =
      emptyObj
        & #frame % #duration ?~ 0
        & #frame % #redraw ?~ False
        & #mode ?~ AnimationModeImmediate
        & #transition % #duration ?~ 0

-- | A slider with a step for each named frame, which jumps to the frame
-- with these options (in @immediate@ mode unless they set one). Frames
-- without a name are skipped.
framesSlider :: Animation -> [Frame] -> Slider
framesSlider anim fs =
  emptyObj
    & #active ?~ 0
    & #steps ?~ map step (mapMaybe (^. #name) fs)
    & #x ?~ 0.1
    & #y ?~ 0
    & #len ?~ 0.9
    & #xanchor ?~ AnnotationXanchorLeft
    & #yanchor ?~ AnnotationYanchorTop
    & #pad % #b ?~ 10
    & #pad % #t ?~ 50
  where
    anim' = anim & #mode %~ (<|> Just AnimationModeImmediate)
    step :: Text -> SliderStep
    step n =
      emptyObj
        & #label ?~ n
        & #method ?~ MethodAnimate
        & #args ?~ [toJSON [n], toJSON anim']
