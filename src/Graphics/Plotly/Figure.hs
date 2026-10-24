{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
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

    -- * Animation
    frame,
    addFrames,
    animationButton,
    playPauseMenu,
    framesSlider,
  )
where

import Data.Aeson (ToJSON, Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Char (isDigit)
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Scientific (toRealFloat)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (LocalTime (..), addLocalTime, diffLocalTime, midnight)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Graphics.Plotly.Animation ()
import Graphics.Plotly.Core
import Graphics.Plotly.Enums (AnnotationXanchor (..), AnnotationYanchor (..), Method (..), UpdatemenuDirection (..), UpdatemenuType (..))
import Graphics.Plotly.Frame ()
import Graphics.Plotly.Layout ()
import Graphics.Plotly.Types (SubplotId (..))
import Optics.Core

-- * Traces

-- | Add a trace at the end.
addTrace :: Trace t -> Figure -> Figure
addTrace tr = #data %~ (<> [someTrace tr])

-- | Add traces at the end.
addTraces :: [SomeTrace] -> Figure -> Figure
addTraces trs = #data %~ (<> trs)

-- | Which of a figure's traces to work on. @a <> b@ selects the traces
-- that both select, and 'mempty' selects them all.
newtype Selector = Selector (Maybe Grid -> Int -> SomeTrace -> Bool)

instance Semigroup Selector where
  Selector a <> Selector b = Selector (\g i t -> a g i t && b g i t)

instance Monoid Selector where
  mempty = Selector (\_ _ _ -> True)

-- | Traces of type @t@: @ofType \@Bar@.
ofType :: forall t. (KnownTraceType t) => Selector
ofType = Selector (\_ _ t -> someTraceType t == Just (traceTypeOf (Proxy @t)))

-- | The trace at this position in the figure's list, from 0.
atIndex :: Int -> Selector
atIndex n = Selector (\_ i _ -> i == n)

-- | Traces with this @name@.
named :: Text -> Selector
named n = withAttr "name" (String n)

-- | Traces in these cells of the figure's grid, on either y axis. In a
-- figure with no grid, no trace.
inCells :: Cells -> Selector
inCells cs = Selector $ \g _ t -> case g of
  Nothing -> False
  Just g' -> maybe False (`elem` concat [cellRefs cell | (_, _, cell) <- selectCells g' cs]) (traceRef t)

-- | Traces on a secondary y axis of the figure's grid.
onSecondaryY :: Selector
onSecondaryY = Selector $ \g _ t -> case g of
  Nothing -> False
  Just g' -> maybe False (`elem` [r | (_, _, cell) <- selectCells g' allCells, Just r <- [cellSecondaryY cell]]) (traceRef t)

-- | Traces for which the function is 'True'.
where_ :: (SomeTrace -> Bool) -> Selector
where_ p = Selector (\_ _ t -> p t)

-- | Traces whose attribute at the dotted path has this value, as with
-- plotly.py's dictionary selectors.
withAttr :: Text -> Value -> Selector
withAttr path v = where_ (\t -> t ^. attr path == Just v)

-- | The selected traces.
traces :: Selector -> Traversal' Figure SomeTrace
traces (Selector p) = traversalVL $ \f fig ->
  let go (i, t)
        | p (grid fig) i t = f t
        | otherwise = pure t
   in (\ts -> fig {data_ = ts}) <$> traverse go (zip [0 ..] (data_ fig))

-- | The selected traces of type @t@, typed: @tracesOf \@Scatter mempty@.
tracesOf :: forall t. (KnownTraceType t) => Selector -> Traversal' Figure (Trace t)
tracesOf sel = traces (ofType @t <> sel) % traversalVL (\f t -> maybe (pure t) (fmap someTrace . f) (fromSomeTrace t))

-- | The selected traces, in order.
selectTraces :: Selector -> Figure -> [SomeTrace]
selectTraces sel = toListOf (traces sel)

-- | Update the selected traces.
updateTraces :: Selector -> (SomeTrace -> SomeTrace) -> Figure -> Figure
updateTraces sel = over (traces sel)

-- | Update the selected traces of type @t@, with their typed optics:
-- @updateTracesOf \@Bar mempty (#opacity ?~ 0.5)@.
updateTracesOf :: forall t. (KnownTraceType t) => Selector -> (Trace t -> Trace t) -> Figure -> Figure
updateTracesOf sel = over (tracesOf @t sel)

-- | Run an action on each selected trace, in order, and replace it with
-- the result.
forEachTrace :: (Applicative f) => Selector -> (SomeTrace -> f SomeTrace) -> Figure -> f Figure
forEachTrace sel = traverseOf (traces sel)

-- * Cells

-- | Cells of a figure's subplot grid, by row and column, counted from 1.
data Cells = Cells (Maybe Int) (Maybe Int)
  deriving stock (Eq, Show)

-- | Every cell.
allCells :: Cells
allCells = Cells Nothing Nothing

-- | The cells in a row.
inRow :: Int -> Cells
inRow r = Cells (Just r) Nothing

-- | The cells in a column.
inCol :: Int -> Cells
inCol c = Cells Nothing (Just c)

-- | One cell.
atCell :: Int -> Int -> Cells
atCell r c = Cells (Just r) (Just c)

-- | The selected cells that hold a subplot, by row, with their rows and
-- columns.
selectCells :: Grid -> Cells -> [(Int, Int, Cell)]
selectCells g (Cells mr mc) =
  [ (r, c, cell)
  | (r, row) <- zip [1 ..] (gridCells g),
    maybe True (== r) mr,
    (c, Just cell) <- zip [1 ..] row,
    maybe True (== c) mc
  ]

cellRefs :: Cell -> [SubplotRef]
cellRefs cell = cellRef cell : maybe [] pure (cellSecondaryY cell)

-- | The subplot a trace is on, as a cell would refer to it.
traceRef :: SomeTrace -> Maybe SubplotRef
traceRef t = case traceSubplotKind <$> someTraceType t of
  Just SubplotXY -> Just (AxesRef (SubplotId (text "xaxis" "x")) (SubplotId (text "yaxis" "y")))
  Just SubplotDomain -> DomainRef <$> pair "domain.x" <*> pair "domain.y"
  Just SubplotScene -> Just (NamedRef (SubplotId (text "scene" "scene")))
  Just SubplotGeo -> Just (NamedRef (SubplotId (text "geo" "geo")))
  Just SubplotPolar -> Just (NamedRef (SubplotId (text "subplot" "polar")))
  Just SubplotTernary -> Just (NamedRef (SubplotId (text "subplot" "ternary")))
  Just SubplotMap -> Just (NamedRef (SubplotId (text "subplot" "map")))
  Just SubplotSmith -> Just (NamedRef (SubplotId (text "subplot" "smith")))
  _ -> Nothing
  where
    text k d = case t ^. attr k of
      Just (String s) -> s
      _ -> d
    pair k = case t ^. attr k of
      Just (Array a) | [Number lo, Number hi] <- foldr (:) [] a -> Just (toRealFloat lo, toRealFloat hi)
      _ -> Nothing

-- * Layout and axes

-- | Update the layout.
updateLayout :: (Layout -> Layout) -> Figure -> Figure
updateLayout = over #layout

-- | The x axes of the selected cells. With 'allCells', or in a figure with
-- no grid, every x axis in the layout, or @xaxis@ if there are none.
xaxes :: Cells -> Traversal' Figure XAxis
xaxes = axes "x" (\case AxesRef (SubplotId x) _ -> [x]; _ -> [])

-- | The y axes of the selected cells, secondary ones included. With
-- 'allCells', or in a figure with no grid, every y axis in the layout, or
-- @yaxis@ if there are none.
yaxes :: Cells -> Traversal' Figure YAxis
yaxes = axes "y" (\case AxesRef _ (SubplotId y) -> [y]; _ -> [])

axes :: Text -> (SubplotRef -> [Text]) -> Cells -> Traversal' Figure (Obj path)
axes xy fromRef cs = traversalVL $ \f fig ->
  let lay = layout fig
      inLayout = [k | k <- KM.keys (toObject lay), isAxisKey (Key.toText k)]
      ks = case grid fig of
        Just g
          | cs /= allCells ->
              nub [axisKeyOf a | (_, _, cell) <- selectCells g cs, ref <- cellRefs cell, a <- fromRef ref]
        _
          | null inLayout -> [Key.fromText (xy <> "axis")]
          | otherwise -> sortOn axisNumber inLayout
      -- Each axis is read from the original layout and written in turn.
      layout' = foldl (\acc k -> (\l v -> l & nested k .~ v) <$> acc <*> f (lay ^. nested k)) (pure lay) ks
   in (\l -> fig {layout = l}) <$> (if null ks then pure lay else layout')
  where
    isAxisKey k = case T.stripPrefix (xy <> "axis") k of
      Just n -> T.all isDigit n
      Nothing -> False
    axisNumber k = case T.stripPrefix (xy <> "axis") (Key.toText k) of
      Just n | not (T.null n) -> read (T.unpack n) :: Int
      _ -> 1

-- | The layout key of an axis id: @"x2"@ is @xaxis2@.
axisKeyOf :: Text -> KM.Key
axisKeyOf ax = Key.fromText (T.take 1 ax <> "axis" <> T.drop 1 ax)

-- | Update the x axes of the selected cells (see 'xaxes').
updateXAxes :: Cells -> (XAxis -> XAxis) -> Figure -> Figure
updateXAxes cs = over (xaxes cs)

-- | Update the y axes of the selected cells (see 'yaxes').
updateYAxes :: Cells -> (YAxis -> YAxis) -> Figure -> Figure
updateYAxes cs = over (yaxes cs)

-- * Shapes and annotations

-- | Add a shape at the end of @layout.shapes@.
addShape :: Shape -> Figure -> Figure
addShape s = #layout % #shapes %~ (Just . (<> [s]) . fromMaybe [])

-- | Add an annotation at the end of @layout.annotations@.
addAnnotation :: Annotation -> Figure -> Figure
addAnnotation a = #layout % #annotations %~ (Just . (<> [a]) . fromMaybe [])

-- | Add a shape to each selected cartesian cell of the figure's grid,
-- referring to its axes (@xref@ and @yref@). A reference ending in
-- @domain@ keeps doing so: @"x domain"@ becomes @"x2 domain"@. A @yref@ set
-- to another axis is kept. Other cells are skipped, and in a figure with no
-- grid the shape is added once, as it is.
addShapeAt :: Cells -> Shape -> Figure -> Figure
addShapeAt cs s fig = foldl (flip addShape) fig (placeAt cs s fig)

-- | Add an annotation to each selected cartesian cell, as 'addShapeAt'
-- adds a shape.
addAnnotationAt :: Cells -> Annotation -> Figure -> Figure
addAnnotationAt cs a fig = foldl (flip addAnnotation) fig (placeAt cs a fig)

-- | Copies of a shape or annotation, one per selected cartesian cell.
placeAt :: (IsObject s) => Cells -> s -> Figure -> [s]
placeAt cs s fig = case grid fig of
  Nothing -> [s]
  Just g -> [withRefs x y s | (_, _, Cell {cellRef = AxesRef (SubplotId x) (SubplotId y)}) <- selectCells g cs]

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
data SpanOptions = SpanOptions
  { -- | The cells to draw in. In a figure with no grid, the shape is drawn
    -- once, on the @x@ and @y@ axes.
    cells :: Cells,
    -- | Skip cells with nothing in them: no traces, shapes, annotations or
    -- images on their axes.
    excludeEmptySubplots :: Bool,
    -- | Set the shape's attributes: @#line % #dash ?~ "dot"@, ….
    shapeStyle :: Shape -> Shape,
    -- | An annotation to go with each shape, if any:
    -- @Just (#text ?~ "limit")@. Its position is set from
    -- 'annotationPosition' unless the function sets it.
    annotation :: Maybe (Annotation -> Annotation),
    -- | Where the annotation goes, as in plotly.py: for a line, @top@,
    -- @bottom@, @left@ or @right@, or a corner such as @top right@; for a
    -- rectangle, the same with @inside@ (the default) or @outside@.
    annotationPosition :: Text
  }

-- | All cells, skipping empty ones, with no styling or annotation.
defaultSpanOptions :: SpanOptions
defaultSpanOptions = SpanOptions allCells True id Nothing "top right"

-- | A horizontal line at this y, across the whole width of each subplot.
addHLine :: (ToJSON a) => SpanOptions -> a -> Figure -> Figure
addHLine opts y = spanning HLine opts (Number 0, Number 1, toJSON y, toJSON y)

-- | A vertical line at this x, across the whole height of each subplot.
addVLine :: (ToJSON a) => SpanOptions -> a -> Figure -> Figure
addVLine opts x = spanning VLine opts (toJSON x, toJSON x, Number 0, Number 1)

-- | A rectangle between these y values, across the whole width of each
-- subplot.
addHRect :: (ToJSON a) => SpanOptions -> a -> a -> Figure -> Figure
addHRect opts y0 y1 = spanning HRect opts (Number 0, Number 1, toJSON y0, toJSON y1)

-- | A rectangle between these x values, across the whole height of each
-- subplot.
addVRect :: (ToJSON a) => SpanOptions -> a -> a -> Figure -> Figure
addVRect opts x0 x1 = spanning VRect opts (toJSON x0, toJSON x1, Number 0, Number 1)

data Spanning = HLine | VLine | HRect | VRect
  deriving stock (Eq)

-- | Add an axis-spanning shape, and its annotation, as plotly.py's
-- @_process_multiple_axis_spanning_shapes@ does.
spanning :: Spanning -> SpanOptions -> (Value, Value, Value, Value) -> Figure -> Figure
spanning kind opts (x0, x1, y0, y1) fig =
  foldl (flip addAnnotation) (foldl (flip addShape) fig shapes) annotations
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
    placedShapes = case grid fig of
      Nothing -> [withDefaultRefs styled]
      Just g -> [withRefs x y styled | (x, y) <- targets g]
    targets g =
      [ (x, y)
      | (_, _, Cell {cellRef = AxesRef (SubplotId x) (SubplotId y)}) <- selectCells g (cells opts),
        not (excludeEmptySubplots opts) || notEmpty x y
      ]
    shapes = map toDomain placedShapes
    annotations = case annotation opts of
      Nothing -> []
      Just f ->
        let a = positioned (f emptyObj)
         in map toDomain $ case grid fig of
              Nothing -> [withDefaultRefs a]
              Just g -> [withRefs x y (a & attr "yref" .~ Nothing) | (x, y) <- targets g]
    positioned :: Annotation -> Annotation
    positioned a = foldl (\acc (k, v) -> if isMissing k acc then acc & attr k ?~ v else acc) a (annotationParams kind (annotationPosition opts) (x0, x1, y0, y1))
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
    notEmpty x y =
      any (on "xaxis" "yaxis" . toObject) (data_ fig)
        || any (on "xref" "yref" . toObject) (layout fig ^. #shapes % non [])
        || any (on "xref" "yref" . toObject) (layout fig ^. #annotations % non [])
        || any (on "xref" "yref") images
      where
        on kx ky o = (str kx "x" o, str ky "y" o) == (x, y)
        str k d o = case KM.lookup k o of
          Just (String s) -> s
          _ -> d
    images = case layout fig ^. attr "images" of
      Just (Array a) -> [o | Object o <- foldr (:) [] a]
      _ -> []

-- | The annotation's @x@, @y@, @xanchor@, @yanchor@ and @showarrow@ for a
-- position, as plotly.py's @shapeannotation@ computes them.
annotationParams :: Spanning -> Text -> (Value, Value, Value, Value) -> [(Text, Value)]
annotationParams kind position (x0, x1, y0, y1)
  | kind `elem` [HLine, VLine] = lineParams
  | otherwise = rectParams
  where
    pos = Set.fromList (T.words position)
    is ws = pos == Set.fromList ws
    anno xa ya x y = [("xanchor", String xa), ("yanchor", String ya), ("x", x), ("y", y), ("showarrow", Bool False)]
    xs = (x0, x1)
    ys = (y0, y1)
    invalid = error ("Graphics.Plotly.Figure: invalid annotation position " <> show position)
    lineParams
      | kind == VLine =
          if
            | is ["top", "left"] -> anno "right" "top" (pick (argmax ys) xs) (maxV ys)
            | is ["top", "right"] -> anno "left" "top" (pick (argmax ys) xs) (maxV ys)
            | is ["top"] -> anno "center" "bottom" (pick (argmax ys) xs) (maxV ys)
            | is ["bottom", "left"] -> anno "right" "bottom" (pick (argmin ys) xs) (minV ys)
            | is ["bottom", "right"] -> anno "left" "bottom" (pick (argmin ys) xs) (minV ys)
            | is ["bottom"] -> anno "center" "top" (pick (argmin ys) xs) (minV ys)
            | is ["left"] -> anno "right" "middle" (meanV xs) (meanV ys)
            | is ["right"] -> anno "left" "middle" (meanV xs) (meanV ys)
            | otherwise -> invalid
      | otherwise =
          if
            | is ["top", "left"] -> anno "left" "bottom" (minV xs) (pick (argmin xs) ys)
            | is ["top", "right"] -> anno "right" "bottom" (maxV xs) (pick (argmax xs) ys)
            | is ["top"] -> anno "center" "bottom" (meanV xs) (meanV ys)
            | is ["bottom", "left"] -> anno "left" "top" (minV xs) (pick (argmin xs) ys)
            | is ["bottom", "right"] -> anno "right" "top" (maxV xs) (pick (argmax xs) ys)
            | is ["bottom"] -> anno "center" "top" (meanV xs) (meanV ys)
            | is ["left"] -> anno "right" "middle" (minV xs) (pick (argmin xs) ys)
            | is ["right"] -> anno "left" "middle" (maxV xs) (pick (argmax xs) ys)
            | otherwise -> invalid
    rectParams =
      let p = if Set.member "inside" pos || Set.member "outside" pos then pos else Set.insert "inside" pos
          isAt ws = p == Set.fromList ws
          v = kind == VRect
          h = kind == HRect
       in if
            | isAt ["inside", "top", "left"] -> anno "left" "top" (minV xs) (maxV ys)
            | isAt ["inside", "top", "right"] -> anno "right" "top" (maxV xs) (maxV ys)
            | isAt ["inside", "top"] -> anno "center" "top" (meanV xs) (maxV ys)
            | isAt ["inside", "bottom", "left"] -> anno "left" "bottom" (minV xs) (minV ys)
            | isAt ["inside", "bottom", "right"] -> anno "right" "bottom" (maxV xs) (minV ys)
            | isAt ["inside", "bottom"] -> anno "center" "bottom" (meanV xs) (minV ys)
            | isAt ["inside", "left"] -> anno "left" "middle" (minV xs) (meanV ys)
            | isAt ["inside", "right"] -> anno "right" "middle" (maxV xs) (meanV ys)
            | isAt ["inside"] -> anno "center" "middle" (meanV xs) (meanV ys)
            | isAt ["outside", "top", "left"] -> anno (if v then "right" else "left") (if h then "bottom" else "top") (minV xs) (maxV ys)
            | isAt ["outside", "top", "right"] -> anno (if v then "left" else "right") (if h then "bottom" else "top") (maxV xs) (maxV ys)
            | isAt ["outside", "top"] -> anno "center" "bottom" (meanV xs) (maxV ys)
            | isAt ["outside", "bottom", "left"] -> anno (if v then "right" else "left") (if h then "top" else "bottom") (minV xs) (minV ys)
            | isAt ["outside", "bottom", "right"] -> anno (if v then "left" else "right") (if h then "top" else "bottom") (maxV xs) (minV ys)
            | isAt ["outside", "bottom"] -> anno "center" "top" (meanV xs) (minV ys)
            | isAt ["outside", "left"] -> anno "right" "middle" (minV xs) (meanV ys)
            | isAt ["outside", "right"] -> anno "left" "middle" (maxV xs) (meanV ys)
            | otherwise -> invalid

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

-- | The middle of two coordinates: numbers, or dates (given as
-- @YYYY-MM-DD@ or @YYYY-MM-DD HH:MM:SS@, the result in the second form).
meanV :: (Value, Value) -> Value
meanV (a, b)
  | a == b = a
  | Number m <- a, Number n <- b = toJSON ((toRealFloat m + toRealFloat n) / 2 :: Double)
  | Just s <- date a,
    Just t <- date b =
      String (T.pack (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (addLocalTime (diffLocalTime t s / 2) s)))
  | otherwise = error ("Graphics.Plotly.Figure: cannot find the middle of " <> show a <> " and " <> show b <> "; give numbers or dates")
  where
    date = \case
      String str ->
        let str' = T.unpack (T.replace " " "T" (T.strip str))
         in case iso8601ParseM str' of
              Just lt -> Just lt
              Nothing -> (`LocalTime` midnight) <$> iso8601ParseM str'
      _ -> Nothing

-- * Animation

-- | A frame with a name and traces. 'framesSlider' and 'animationButton'
-- refer to frames by name.
frame :: Text -> [SomeTrace] -> Frame
frame n trs = emptyObj & #name ?~ n & #data ?~ trs

-- | Add frames at the end.
addFrames :: [Frame] -> Figure -> Figure
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
