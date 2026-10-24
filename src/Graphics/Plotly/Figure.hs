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

import Data.Aeson (Result (..), ToJSON, Value (..), fromJSON, object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isDigit)
import Data.Foldable (asum, toList)
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
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
import Graphics.Plotly.Enums (AnnotationXanchor (..), AnnotationYanchor (..), Method (..), UpdatemenuDirection (..), UpdatemenuType (..))
import Graphics.Plotly.Frame ()
import Graphics.Plotly.Grid (CheckCell, CheckCell', CheckCol, CheckCol', CheckRow, CheckRow', check, natInt)
import Graphics.Plotly.Internal.Core
import Graphics.Plotly.Layout ()
import Graphics.Plotly.Subplots (cellPosition)
import Graphics.Plotly.Types (SubplotId (..))
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
newtype Selector (g :: FigureGrid) = Selector (Maybe GridInfo -> Int -> SomeTrace -> Bool)

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

-- | Traces in these cells of the figure's grid, on either y axis.
inCells :: Cells ('Grid g) -> Selector ('Grid g)
inCells cs = Selector $ \g ->
  let refs = foldMap (\g' -> concatMap cellRefs (selectCells g' cs)) g
   in \_ t -> maybe False (`elem` refs) (traceRef t)

-- | Traces on a secondary y axis of the figure's grid.
onSecondaryY :: Selector ('Grid g)
onSecondaryY = Selector $ \g ->
  let refs = foldMap (\g' -> [r | cell <- selectCells g' allCells, Just r <- [cellSecondaryY cell]]) g
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
-- time.
data Cells (g :: FigureGrid) = Cells (Maybe Int) (Maybe Int)
  deriving stock (Eq, Show)

type role Cells nominal

-- | Every cell; for a figure with no grid, the whole figure.
allCells :: Cells g
allCells = Cells Nothing Nothing

-- | The cells in row @r@: @inRow \@2@.
inRow :: forall r g. (KnownNat r, CheckRow g r) => Cells ('Grid g)
inRow = check @(CheckRow' g r) (Cells (Just (natInt @r)) Nothing)

-- | The cells in column @c@.
inCol :: forall c g. (KnownNat c, CheckCol g c) => Cells ('Grid g)
inCol = check @(CheckCol' g c) (Cells Nothing (Just (natInt @c)))

-- | The cell at row @r@ and column @c@, which must hold a subplot.
atCell :: forall r c g. (KnownNat r, KnownNat c, CheckCell g r c) => Cells ('Grid g)
atCell = check @(CheckCell' g r c) (Cells (Just (natInt @r)) (Just (natInt @c)))

-- | The cell a cell index names.
cellsOf :: CellIx g k y2 -> Cells ('Grid g)
cellsOf i = let (r, c) = cellPosition i in Cells (Just r) (Just c)

-- | The selected cells that hold a subplot, by row.
selectCells :: GridInfo -> Cells g -> [CellInfo]
selectCells g (Cells mr mc) =
  [ cell
  | (r, row) <- zip [1 ..] (gridCells g),
    maybe True (== r) mr,
    (c, Just cell) <- zip [1 ..] row,
    maybe True (== c) mc
  ]

-- | The subplot a trace is on, as a cell would refer to it.
traceRef :: SomeTrace -> Maybe SubplotRef
traceRef t =
  someTraceType t >>= traceSubplotKind >>= \case
    SubplotXY -> Just (AxesRef (SubplotId (text "xaxis" "x")) (SubplotId (text "yaxis" "y")))
    SubplotDomain -> DomainRef <$> pair "domain.x" <*> pair "domain.y"
    SubplotNamed k -> Just (NamedRef k (SubplotId (text (subplotAttr k) (subplotName k))))
  where
    text k d = case t ^. attr k of
      Just (String s) -> s
      _ -> d
    pair k = case fromJSON <$> t ^. attr k of
      Just (Success p) -> Just p
      _ -> Nothing

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
      inLayout = [k | k <- KM.keys (toObject lay), isAxisKey (Key.toText k)]
      ks = case figGrid fig of
        Just g
          | cs /= allCells ->
              nub [axisKeyOf a | cell <- selectCells g cs, ref <- cellRefs cell, a <- fromRef ref]
        _
          | null inLayout -> [Key.fromText (xy <> "axis")]
          | otherwise -> sortOn axisNumber inLayout
      -- Each axis is read from the original layout and written in turn.
      layout' = foldl (\acc k -> (\l v -> l & nested k .~ v) <$> acc <*> f (lay ^. nested k)) (pure lay) ks
   in (\l -> fig {layout = l}) <$> layout'
  where
    isAxisKey k = case T.stripPrefix (xy <> "axis") k of
      Just n -> T.all isDigit n
      Nothing -> False
    axisNumber k = case T.stripPrefix (xy <> "axis") (Key.toText k) of
      Just n | not (T.null n) -> read (T.unpack n) :: Int
      _ -> 1

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
placeAt :: (IsObject s) => Cells g -> s -> Figure g -> [s]
placeAt cs s fig = case figGrid fig of
  Nothing -> [s]
  Just g -> [withRefs x y s | CellInfo {cellRef = AxesRef (SubplotId x) (SubplotId y)} <- selectCells g cs]

-- | Set @xref@ and @yref@ to these axes, keeping @domain@ suffixes, and
-- keeping a @yref@ to some other axis.
withRefs :: (IsObject s) => Text -> Text -> s -> s
withRefs x y s =
  s
    & attr "xref" ?~ String (suffixed (ref "xref") x)
    & attr "yref" ?~ String (if ownY then suffixed (ref "yref") y else fromMaybe y (ref "yref"))
  where
    ref k = case s ^. attr k of
      Just (String r) -> Just r
      _ -> Nothing
    ownY = case ref "yref" of
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
        & attr "type" ?~ String (if kind `elem` [HLine, VLine] then "line" else "rect")
        & attr "x0" ?~ x0
        & attr "x1" ?~ x1
        & attr "y0" ?~ y0
        & attr "y1" ?~ y1
    styled = shapeStyle opts base
    placedShapes = case figGrid fig of
      Nothing -> [withDefaultRefs styled]
      Just _ -> [withRefs x y styled | (x, y) <- targets]
    targets =
      [ (x, y)
      | Just g <- [figGrid fig],
        CellInfo {cellRef = AxesRef (SubplotId x) (SubplotId y)} <- selectCells g (cells opts),
        not (excludeEmptySubplots opts) || (x, y) `Set.member` occupied
      ]
    shapes = map toDomain placedShapes
    -- The annotation goes on the shape's y axis, as plotly.py puts it.
    annotations = case annotation opts of
      Nothing -> []
      Just f ->
        let a = positioned (f emptyObj) & attr "yref" .~ (styled ^. attr "yref")
         in map toDomain $ case figGrid fig of
              Nothing -> [withDefaultRefs a]
              Just _ -> [withRefs x y a | (x, y) <- targets]
    positioned :: Annotation -> Annotation
    positioned a = foldl (\acc (k, v) -> if isMissing k acc then acc & attr k ?~ v else acc) a (annotationParams kind (annotationPosition opts) (annotationOutside opts) (x0, x1, y0, y1))
    isMissing k a = case a ^. attr k of
      Nothing -> True
      Just Null -> True
      Just _ -> False
    withDefaultRefs :: (IsObject s) => s -> s
    withDefaultRefs s =
      s
        & attr "xref" %~ Just . fromMaybe (String "x")
        & attr "yref" %~ Just . fromMaybe (String "y")
    toDomain :: (IsObject s) => s -> s
    toDomain = attr (if horizontal then "xref" else "yref") %~ fmap (\case String r -> String (r <> " domain"); v -> v)
    -- The axes that something is drawn on.
    occupied =
      Set.fromList $
        map (axesOf "xaxis" "yaxis" . toObject) (data_ fig)
          <> map (axesOf "xref" "yref" . toObject) (layout fig ^. #shapes % non [])
          <> map (axesOf "xref" "yref" . toObject) (layout fig ^. #annotations % non [])
          <> map (axesOf "xref" "yref") images
    axesOf kx ky o = (str kx "x" o, str ky "y" o)
    str k d o = case KM.lookup k o of
      Just (String s) -> s
      _ -> d
    images = case layout fig ^. attr "images" of
      Just (Array a) -> [o | Object o <- toList a]
      _ -> []

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

-- | The annotation's @x@, @y@, @xanchor@, @yanchor@ and @showarrow@ for a
-- position, as plotly.py's @shapeannotation@ computes them. A rectangle's
-- annotation is outside it for 'True'.
annotationParams :: Spanning -> AnnotationPosition -> Bool -> (Value, Value, Value, Value) -> [(Text, Value)]
annotationParams kind position outside (x0, x1, y0, y1) = case kind of
  VLine -> case position of
    AtTopLeft -> anno "right" "top" (pick (argmax ys) xs) (maxV ys)
    AtTopRight -> anno "left" "top" (pick (argmax ys) xs) (maxV ys)
    AtTop -> anno "center" "bottom" (pick (argmax ys) xs) (maxV ys)
    AtBottomLeft -> anno "right" "bottom" (pick (argmin ys) xs) (minV ys)
    AtBottomRight -> anno "left" "bottom" (pick (argmin ys) xs) (minV ys)
    AtBottom -> anno "center" "top" (pick (argmin ys) xs) (minV ys)
    AtLeft -> anno "right" "middle" (meanV xs) (meanV ys)
    AtRight -> anno "left" "middle" (meanV xs) (meanV ys)
    AtCenter -> centre
  HLine -> case position of
    AtTopLeft -> anno "left" "bottom" (minV xs) (pick (argmin xs) ys)
    AtTopRight -> anno "right" "bottom" (maxV xs) (pick (argmax xs) ys)
    AtTop -> anno "center" "bottom" (meanV xs) (meanV ys)
    AtBottomLeft -> anno "left" "top" (minV xs) (pick (argmin xs) ys)
    AtBottomRight -> anno "right" "top" (maxV xs) (pick (argmax xs) ys)
    AtBottom -> anno "center" "top" (meanV xs) (meanV ys)
    AtLeft -> anno "right" "middle" (minV xs) (pick (argmin xs) ys)
    AtRight -> anno "left" "middle" (maxV xs) (pick (argmax xs) ys)
    AtCenter -> centre
  _ | not outside -> case position of
    AtTopLeft -> anno "left" "top" (minV xs) (maxV ys)
    AtTopRight -> anno "right" "top" (maxV xs) (maxV ys)
    AtTop -> anno "center" "top" (meanV xs) (maxV ys)
    AtBottomLeft -> anno "left" "bottom" (minV xs) (minV ys)
    AtBottomRight -> anno "right" "bottom" (maxV xs) (minV ys)
    AtBottom -> anno "center" "bottom" (meanV xs) (minV ys)
    AtLeft -> anno "left" "middle" (minV xs) (meanV ys)
    AtRight -> anno "right" "middle" (maxV xs) (meanV ys)
    AtCenter -> centre
  _ -> case position of
    AtTopLeft -> anno (if v then "right" else "left") (if h then "bottom" else "top") (minV xs) (maxV ys)
    AtTopRight -> anno (if v then "left" else "right") (if h then "bottom" else "top") (maxV xs) (maxV ys)
    AtTop -> anno "center" "bottom" (meanV xs) (maxV ys)
    AtBottomLeft -> anno (if v then "right" else "left") (if h then "top" else "bottom") (minV xs) (minV ys)
    AtBottomRight -> anno (if v then "left" else "right") (if h then "top" else "bottom") (maxV xs) (minV ys)
    AtBottom -> anno "center" "top" (meanV xs) (minV ys)
    AtLeft -> anno "right" "middle" (minV xs) (meanV ys)
    AtRight -> anno "left" "middle" (maxV xs) (meanV ys)
    AtCenter -> centre
  where
    anno xa ya x y = [("xanchor", String xa), ("yanchor", String ya), ("x", x), ("y", y), ("showarrow", Bool False)]
    centre = anno "center" "middle" (meanV xs) (meanV ys)
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
        & #args
          ?~ [ toJSON [Null],
               object
                 [ "frame" .= object ["duration" .= (0 :: Int), "redraw" .= False],
                   "mode" .= ("immediate" :: Text),
                   "transition" .= object ["duration" .= (0 :: Int)]
                 ]
             ]

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
    anim' = anim & attr "mode" %~ Just . fromMaybe (String "immediate")
    step :: Text -> SliderStep
    step n =
      emptyObj
        & #label ?~ n
        & #method ?~ MethodAnimate
        & #args ?~ [toJSON [n], toJSON anim']
