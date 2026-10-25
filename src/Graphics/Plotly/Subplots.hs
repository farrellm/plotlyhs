{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Grids of subplots, as plotly.py's @plotly.subplots.make_subplots@ makes
-- them, checked by the type checker. The grid is a type (see
-- "Graphics.Plotly.Grid"); 'makeSubplots' lays out its axes, scenes, polar
-- plots and so on, and 'addTraceAt' puts a trace in a cell:
--
-- @
-- fig :: Figure ('Grid '[ '[XY, Domain]])
-- fig =
--   makeSubplots \@'[ '[XY, Domain]] subplotOptions {subplotTitles = [\"Line\", \"Pie\"]}
--     & addTraceAt \@1 \@1 (scatter & #y ?~ dataArray [1, 3, 2 :: Double])
--     & addTraceAt \@1 \@2 (pie & #values ?~ dataArray [2, 1 :: Double])
-- @
--
-- A cell outside the grid or blank, a trace on the wrong kind of subplot
-- (a pie on an xy cell), or a secondary y axis that is not there, is a type
-- error. The layout is the same as plotly.py's, so figures look alike in
-- both.
module Graphics.Plotly.Subplots
  ( -- * Making a grid
    makeSubplots,
    SubplotOptions (..),
    subplotOptions,
    Padding (..),
    noPadding,
    Sharing (..),
    StartCell (..),
    module Graphics.Plotly.Grid,

    -- * Placing traces
    addTraceAt,
    addTraceSecondaryY,

    -- ** Cells as values
    CellIx,
    cell,
    cellY2,
    cellPosition,
    addTraceIn,

    -- * Grids sized at runtime
    withUniformSubplots,

    -- * The grid
    printGrid,
  )
where

import Data.Aeson (toJSON)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.TypeError (ErrorMessage (..))
import GHC.TypeLits (KnownNat)
import Graphics.Plotly.Enums (Anchor (..), AnnotationXanchor (..), AnnotationXref (..), AnnotationYanchor (..), XaxisSide (..))
import Graphics.Plotly.Grid
import Graphics.Plotly.Internal.Core
import Graphics.Plotly.Layout (xaxisN, yaxisN)
import Graphics.Plotly.Types (Angle (..), SubplotId (..))
import Optics.Core

-- | How axes are shared between subplots. A shared axis
-- @matches@ the first in its group, and only the outermost shows its tick
-- labels.
data Sharing
  = NotShared
  | -- | Within each column.
    ShareColumns
  | -- | Within each row.
    ShareRows
  | -- | Across the whole grid.
    ShareAll
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | Space inside a cell around its subplot, as fractions of the plot's
-- width (left and right) or height (top and bottom).
data Padding = Padding
  { padLeft :: Double,
    padRight :: Double,
    padTop :: Double,
    padBottom :: Double
  }
  deriving stock (Eq, Show)

-- | No padding.
noPadding :: Padding
noPadding = Padding 0 0 0 0

-- | How to lay the grid out: the arguments of plotly.py's @make_subplots@
-- other than the grid itself, which is a type. Every combination is
-- allowed: spacing and padding are clamped to what fits.
data SubplotOptions = SubplotOptions
  { sharedXAxes :: Sharing,
    sharedYAxes :: Sharing,
    startCell :: StartCell,
    -- | The space between columns, as a fraction of the plot's width. By
    -- default 0.2 divided by the columns (0.4 with a secondary y axis).
    horizontalSpacing :: Maybe Double,
    -- | The space between rows, as a fraction of the plot's height. By
    -- default 0.3 divided by the rows (0.5 with subplot titles).
    verticalSpacing :: Maybe Double,
    -- | A title over each cell, in order row by row, skipping blank cells.
    -- An empty title is left out.
    subplotTitles :: [Text],
    -- | The relative width of each column, by its number from 1. Equal by
    -- default; a negative width counts as 0.
    columnWidths :: Maybe (Int -> Double),
    -- | The relative height of each row, by its number from 1. Equal by
    -- default; a negative height counts as 0.
    rowHeights :: Maybe (Int -> Double),
    -- | The padding of the cell at a row and column.
    cellPadding :: Int -> Int -> Padding,
    -- | A title over each column.
    columnTitles :: [Text],
    -- | A title to the right of each row.
    rowTitles :: [Text],
    -- | A title under the whole grid.
    xTitle :: Maybe Text,
    -- | A title left of the whole grid.
    yTitle :: Maybe Text
  }

-- | plotly.py's defaults.
subplotOptions :: SubplotOptions
subplotOptions =
  SubplotOptions
    { sharedXAxes = NotShared,
      sharedYAxes = NotShared,
      startCell = TopLeft,
      horizontalSpacing = Nothing,
      verticalSpacing = Nothing,
      subplotTitles = [],
      columnWidths = Nothing,
      rowHeights = Nothing,
      cellPadding = \_ _ -> noPadding,
      columnTitles = [],
      rowTitles = [],
      xTitle = Nothing,
      yTitle = Nothing
    }

-- | An empty figure with the grid of subplots @g@: its axes, scenes, polar
-- plots and so on, with their domains, and its titles as annotations. A
-- malformed grid is a type error (see 'ValidGrid').
makeSubplots :: forall g. (ValidGrid g, KnownGrid g) => SubplotOptions -> Figure ('Grid g)
makeSubplots = check @(CheckShape' g) . check @(CheckSpecs' g) . check @(CheckCover' g (Positions g 1)) . layoutGrid (gridVal @g)

-- | A figure laid out for these cells, which the caller has checked.
layoutGrid :: [[Maybe SpecInfo]] -> SubplotOptions -> Figure ('Grid g)
layoutGrid specGrid opts =
  (figure [])
    { layout = finalLayout,
      figGrid = HasGridInfo (GridInfo (startCell opts) cellGrid)
    }
  where
    nRows = length specGrid
    nCols = maybe 0 length (listToMaybe specGrid)
    bottomUp = startCell opts == BottomLeft
    hasSecondaryY = or [specSecondaryY s | row <- specGrid, Just s <- row]
    hSpacing = spacing maxWidth nCols (fromMaybe ((if hasSecondaryY then 0.4 else 0.2) / fromIntegral nCols) (horizontalSpacing opts))
    vSpacing = spacing 1 nRows (fromMaybe ((if null (subplotTitles opts) then 0.3 else 0.5) / fromIntegral nRows) (verticalSpacing opts))
    maxWidth
      | hasSecondaryY = 0.94
      | not (null (rowTitles opts)) = 0.98
      | otherwise = 1.0
    widths = case weights nCols (columnWidths opts) of
      Nothing -> replicate nCols ((maxWidth - hSpacing * fromIntegral (nCols - 1)) / fromIntegral nCols)
      Just ws -> [(maxWidth - hSpacing * fromIntegral (nCols - 1)) * (w / sum ws) | w <- ws]
    -- Bottom up, whichever corner row 1 is in.
    heights = case weights nRows (rowHeights opts) of
      Nothing -> replicate nRows ((1.0 - vSpacing * fromIntegral (nRows - 1)) / fromIntegral nRows)
      Just hs ->
        (if bottomUp then id else reverse)
          [(1.0 - vSpacing * fromIntegral (nRows - 1)) * (h / sum hs) | h <- hs]
    -- Where each row and column starts, by row (from row 1) and column.
    colStart c = sum (take c widths) + fromIntegral c * hSpacing
    position r = if bottomUp then r else nRows - 1 - r
    rowStart r = sum (take (position r) heights) + fromIntegral (position r) * vSpacing
    -- The type checks that spans stay inside the grid; the indices are
    -- clamped anyway, so that this is total.
    domainOf r c s =
      let c' = min (nCols - 1) (c + specColspan s - 1)
          r' = min (nRows - 1) (r + specRowspan s - 1)
          Padding pl pr pt pb = cellPadding opts (r + 1) (c + 1)
          xs = (colStart c + pl, colStart c' + widths !! c' - pr)
          ys
            | bottomUp = (rowStart r + pb, rowStart r' + heights !! r' - pt)
            | otherwise = (rowStart r' + pb, rowStart r + heights !! position r - pt)
       in (clamp xs, clamp ys)
    -- The cells in order, with their domains.
    placed :: [((Int, Int), SpecInfo, ((Double, Double), (Double, Double)))]
    placed = [((r, c), s, domainOf r c s) | (r, row) <- zip [0 ..] specGrid, (c, Just s) <- zip [0 ..] row]
    -- Subplots are numbered in cell order, by kind: an x axis once per
    -- cell, and a y axis once more for a secondary y.
    (_, cellsAt, baseLayout) = foldl initCell ((1, 1, Map.empty), Map.empty, emptyObj) placed
    -- The fold carries the numbers of the next x axis, y axis and subplot
    -- of each named kind.
    initCell ((x, y, named), refs, lay) (rc, s, (xs, ys)) =
      let cellInfo = CellInfo (specRowspan s) (specColspan s)
          (next, ci, lay') = case specKind s of
            SubplotXY ->
              let xLabel = axisId "x" x
                  yLabel = axisId "y" y
                  secondary = if specSecondaryY s then Just (y + 1) else Nothing
                  anchor = Just . AnchorSubplot . SubplotId
                  ref = AxesRef (SubplotId xLabel) . SubplotId . axisId "y"
               in ( (x + 1, maybe y id secondary + 1, named),
                    cellInfo (ref y) (ref <$> secondary),
                    lay
                      & xaxisN x % #domain ?~ xs
                      & xaxisN x % #anchor .~ anchor yLabel
                      & yaxisN y % #domain ?~ ys
                      & yaxisN y % #anchor .~ anchor xLabel
                      & maybe
                        id
                        (\y2 -> (yaxisN y2 % #anchor .~ anchor xLabel) . (yaxisN y2 % #overlaying .~ anchor yLabel) . (yaxisN y2 % #side ?~ XaxisSideRight))
                        secondary
                  )
            SubplotDomain -> ((x, y, named), cellInfo (DomainRef xs ys) Nothing, lay)
            SubplotNamed k ->
              let n = Map.findWithDefault 1 k named
                  label = axisId (subplotName k) n
               in ( (x, y, Map.insert k (n + 1) named),
                    cellInfo (NamedRef k (SubplotId label)) Nothing,
                    lay & attr (label <> ".domain.x") ?~ toJSON xs & attr (label <> ".domain.y") ?~ toJSON ys
                  )
       in (next, Map.insert rc ci refs, lay')
    cellGrid = [[Map.lookup (r, c) cellsAt | c <- [0 .. nCols - 1]] | r <- [0 .. nRows - 1]]
    -- Shared axes.
    sharedLayout =
      foldl
        (\l (xy, sharing, secondary) -> shareAxes xy sharing secondary l)
        baseLayout
        ([("x", sharedXAxes opts, False), ("y", sharedYAxes opts, False)] <> [("y", sharedYAxes opts, True) | hasSecondaryY])
    shareAxes :: Text -> Sharing -> Bool -> Layout -> Layout
    shareAxes xy sharing secondary lay0 = case sharing of
      NotShared -> lay0
      ShareColumns -> foldl (\l c -> groupMatch [(r, c, xy == "x") | r <- rowsIter] l) lay0 colsIter
      ShareRows -> foldl (\l r -> groupMatch [(r, c, xy == "y") | c <- colsIter] l) lay0 rowsIter
      ShareAll ->
        groupMatch
          [ (r, c, removeLabel)
          | (ri, r) <- zip [0 :: Int ..] rowsIter,
            c <- colsIter,
            let removeLabel
                  | xy == "y" = if secondary then c < nCols - 1 else c > 0
                  | bottomUp = ri > 0
                  | otherwise = r < nRows - 1
          ]
          lay0
      where
        rowsIter = if bottomUp then [0 .. nRows - 1] else [nRows - 1, nRows - 2 .. 0]
        colsIter = if secondary then [nCols - 1, nCols - 2 .. 0] else [0 .. nCols - 1]
        axisOf (r, c) = do
          ci <- Map.lookup (r, c) cellsAt
          ref <- cellRefFor secondary ci
          let span' = if xy == "x" then cellColspan ci else cellRowspan ci
          case ref of
            AxesRef (SubplotId x) (SubplotId y) | span' == 1 -> Just (if xy == "x" then x else y)
            _ -> Nothing
        groupMatch members lay = snd (foldl step (Nothing, lay) members)
          where
            step (first, l) (r, c, removeLabel) = case axisOf (r, c) of
              Nothing -> (first, l)
              Just ax -> case first of
                Nothing -> (Just ax, l)
                Just f
                  | xy == "x" -> (first, l & (nested key :: Lens' Layout XAxis) %~ matchAxis (SubplotId f) removeLabel)
                  | otherwise -> (first, l & (nested key :: Lens' Layout YAxis) %~ matchAxis (SubplotId f) removeLabel)
                  where
                    key = axisKeyOf ax
    -- Titles.
    titleAnnotations =
      titlesAt TopEdge 0 (subplotTitles opts) [d | (_, _, d) <- placed]
        <> titlesAt TopEdge 0 (columnTitles opts) (catMaybes [domainAt (if bottomUp then nRows - 1 else 0) c | c <- [0 .. nCols - 1]])
        <> titlesAt RightEdge 0 (rowTitles opts) (catMaybes [domainAt r (nCols - 1) | r <- [0 .. nRows - 1]])
        <> titlesAt BottomEdge 30 (maybe [] pure (xTitle opts)) [((0, maxWidth), (0, 1))]
        <> titlesAt LeftEdge 40 (maybe [] pure (yTitle opts)) [((0, 1), (0, 1))]
    domainAt r c = Map.lookup (r, c) domains
    domains = Map.fromList [(rc, d) | (rc, _, d) <- placed]
    finalLayout = sharedLayout & #annotations % non [] .~ titleAnnotations

-- | Make an axis match another, and hide its tick labels if asked.
matchAxis :: (LabelOptic' "matches" A_Lens a (Maybe SubplotId), LabelOptic' "showticklabels" A_Lens a (Maybe Bool)) => SubplotId -> Bool -> a -> a
matchAxis f removeLabel a = a & #matches ?~ f & if removeLabel then #showticklabels ?~ False else id

-- | Spacing between @n@ rows or columns, clamped to what fits in the
-- room they share (1, or less for the width beside row titles or a
-- secondary y axis).
spacing :: Double -> Int -> Double -> Double
spacing room n = max 0 . min (if n > 1 then room / fromIntegral (n - 1) else room)

-- | Weights for @n@ rows or columns from a function of their number, none
-- negative; 'Nothing' if there is no function or every weight is 0.
weights :: Int -> Maybe (Int -> Double) -> Maybe [Double]
weights n mf = do
  f <- mf
  let ws = [max 0 (f i) | i <- [1 .. n]]
  if sum ws > 0 then Just ws else Nothing

data Edge = TopEdge | BottomEdge | RightEdge | LeftEdge

-- | Annotations for titles at an edge of each domain, as plotly.py places
-- them, shifted away from it by the offset in pixels. An empty title, or
-- one past the last domain, is left out.
titlesAt :: Edge -> Double -> [Text] -> [((Double, Double), (Double, Double))] -> [Annotation]
titlesAt edge offset titles domains =
  [ emptyObj
      & #x ?~ toJSON x
      & #y ?~ toJSON y
      & #xref ?~ AnnotationXrefPaper
      & #yref ?~ AnnotationXrefPaper
      & #text ?~ t
      & #showarrow ?~ False
      & #font % #size ?~ 16
      & #xanchor ?~ xanchor
      & #yanchor ?~ yanchor
      & #xshift .~ nonZero xshift
      & #yshift .~ nonZero yshift
      & #textangle .~ (Angle <$> nonZero angle)
  | (t, ((x0, x1), (y0, y1))) <- zip titles domains,
    not (T.null t),
    let (x, y, xanchor, yanchor, xshift, yshift, angle) = case edge of
          TopEdge -> ((x0 + x1) / 2, y1, AnnotationXanchorCenter, AnnotationYanchorBottom, 0, offset, 0)
          BottomEdge -> ((x0 + x1) / 2, y0, AnnotationXanchorCenter, AnnotationYanchorTop, 0, -offset, 0)
          RightEdge -> (x1, (y0 + y1) / 2, AnnotationXanchorLeft, AnnotationYanchorMiddle, offset, 0, 90)
          LeftEdge -> (x0, (y0 + y1) / 2, AnnotationXanchorRight, AnnotationYanchorMiddle, -offset, 0, -90) :: (Double, Double, AnnotationXanchor, AnnotationYanchor, Double, Double, Double)
  ]
  where
    nonZero v = if v /= 0 then Just v else Nothing

-- | A domain inside 0 to 1, and never reversed: padding wider than its
-- cell leaves an empty domain.
clamp :: (Double, Double) -> (Double, Double)
clamp (lo, hi) = let lo' = min 1 (max 0 lo) in (lo', max lo' (min 1 hi))

-- | The cell at row @r@ and column @c@, counted from 1. A type error if it
-- is outside the grid or blank.
cell :: forall r c g. (KnownNat r, KnownNat c, CheckCell g r c) => CellIx g (KindAt g r c) 'False
cell = check @(CheckCell' g r c) (CellIx (natInt @r, natInt @c))

-- | The secondary y axis of the cell at row @r@ and column @c@. A type
-- error if the cell is not 'PlotY2'.
cellY2 :: forall r c g. (KnownNat r, KnownNat c, CheckSecondaryY g r c) => CellIx g 'SubplotXY 'True
cellY2 = check @(CheckSecondaryY' g r c) (CellIx (natInt @r, natInt @c))

-- | The cell's row and column.
cellPosition :: CellIx g k y2 -> (Int, Int)
cellPosition (CellIx rc) = rc

-- | Add a trace to the cell at row @r@ and column @c@, counted from 1,
-- setting the attributes that place it there: @xaxis@ and @yaxis@,
-- @scene@, @geo@, @subplot@ or @domain@. A type error if the cell is
-- outside the grid or blank, or holds a kind of subplot the trace is not
-- drawn on.
addTraceAt :: forall r c t g. (KnownNat r, KnownNat c, CanPlace g r c t 'False) => Trace t -> Figure ('Grid g) -> Figure ('Grid g)
addTraceAt = check @(CanPlace' g r c t 'False) (addTo False (natInt @r, natInt @c))

-- | Add a trace to the secondary y axis of the cell at row @r@ and column
-- @c@. A type error as for 'addTraceAt', and if the cell is not 'PlotY2'.
addTraceSecondaryY :: forall r c t g. (KnownNat r, KnownNat c, CanPlace g r c t 'True) => Trace t -> Figure ('Grid g) -> Figure ('Grid g)
addTraceSecondaryY = check @(CanPlace' g r c t 'True) (addTo True (natInt @r, natInt @c))

-- | Add a trace to a cell given as a value. A type error if the trace is
-- not drawn on the cell's kind of subplot.
addTraceIn :: forall t g k y2. (KnownY2 y2, KindMatches t k) => CellIx g k y2 -> Trace t -> Figure ('Grid g) -> Figure ('Grid g)
addTraceIn = check @(KindFits' t k ('Text "the cell")) (addTo (y2Val @y2) . cellPosition)

-- | Add a trace to a checked cell, on its secondary y axis for 'True'.
addTo :: Bool -> (Int, Int) -> Trace t -> Figure ('Grid g) -> Figure ('Grid g)
addTo secondary (r, c) tr f = f & #data %~ (<> [placeTrace secondary r c f (someTrace tr)])

-- | Whether a cell index is for the secondary y axis.
class KnownY2 (y2 :: Bool) where
  y2Val :: Bool

instance KnownY2 'True where y2Val = True

instance KnownY2 'False where y2Val = False

-- | Place a trace in a cell the types have checked. The grid always has
-- the cell; if it somehow did not, the trace would be left where it is.
placeTrace :: Bool -> Int -> Int -> Figure ('Grid g) -> SomeTrace -> SomeTrace
placeTrace secondary r c f tr = case gridCellInfo r c (gridInfo f) >>= cellRefFor secondary of
  Nothing -> tr
  Just (AxesRef x y) -> tr & attr "xaxis" ?~ toJSON x & attr "yaxis" ?~ toJSON y
  Just (NamedRef k n) -> tr & attr (subplotAttr k) ?~ toJSON n
  Just (DomainRef xs ys) -> tr & attr "domain.x" ?~ toJSON xs & attr "domain.y" ?~ toJSON ys

-- | A grid of xy subplots whose size is only known at runtime, with its
-- cells by row: the continuation can place traces only in those cells, so
-- this is as safe as a grid known at compile time. Fewer than 1 row or
-- column counts as 1.
--
-- @
-- facets :: [[Double]] -> Figure 'NoGrid
-- facets series =
--   withUniformSubplots (length series) 1 subplotOptions $ \\fig cells ->
--     forgetGrid (foldl (\\f (ys, [ix]) -> addTraceIn ix (scatter & #y ?~ dataArray ys) f) fig (zip series cells))
-- @
withUniformSubplots :: Int -> Int -> SubplotOptions -> (forall g. Figure ('Grid g) -> [[CellIx g 'SubplotXY 'False]] -> a) -> a
withUniformSubplots r c opts k =
  k
    (layoutGrid @RuntimeGrid (replicate r' (replicate c' (Just (SpecInfo SubplotXY False 1 1)))) opts)
    [[CellIx (i, j) | j <- [1 .. c']] | i <- [1 .. r']]
  where
    r' = max 1 r
    c' = max 1 c

-- | The grid of 'withUniformSubplots', known only at runtime.
type family RuntimeGrid :: [[CellSpec]] where

-- | The grid of a figure, drawn as plotly.py's @print_grid@ draws it:
-- each cell with its row, column and subplots, or @(empty)@.
--
-- @
-- This is the format of your plot grid:
-- [ (1,1) x,y   ]  [ (1,2) x2,y2 ]
-- @
printGrid :: Figure ('Grid g) -> Text
printGrid f =
  "This is the format of your plot grid:\n"
    <> T.concat [T.intercalate "  " [cellText r c | c <- [0 .. nCols - 1]] <> "\n" | r <- printOrder]
  where
    g = gridInfo f
    nRows = length (gridCells g)
    nCols = maybe 0 length (listToMaybe (gridCells g))
    -- Top to bottom on the page.
    printOrder = if gridStartCell g == BottomLeft then [nRows - 1, nRows - 2 .. 0] else [0 .. nRows - 1]
    cells = [((r, c), ci) | (r, row) <- zip [0 ..] (gridCells g), (c, Just ci) <- zip [0 ..] row]
    label r c ci =
      let ids = Set.toAscList (Set.fromList (concatMap refIds (cellRefs ci)))
       in "(" <> tshow (r + 1) <> "," <> tshow (c + 1) <> ") " <> T.intercalate "," ids
    refIds = \case
      AxesRef (SubplotId x) (SubplotId y) -> [x, y]
      NamedRef _ (SubplotId n) -> [n]
      DomainRef _ _ -> []
    cellLen = maximum (0 : [T.length (label r c ci) | ((r, c), ci) <- cells]) + 4
    pad s = T.replicate (cellLen - T.length s) " "
    colspanStr = "       -"
    rowspanStr = "       :"
    emptyStr = "    (empty) "
    -- Each cell's text, as plotly.py fills them in, in order: a spanning
    -- cell writes over the cells it covers.
    texts = foldl place Map.empty cells
    place m ((r, c), ci) =
      let rs = cellRowspan ci
          cs = cellColspan ci
          start = (if rs > 1 then "⎡ " else "[ ") <> label r c ci
          startCell'
            | cs > 1 = start
            | otherwise = start <> T.replicate (cellLen - T.length start - 2) " " <> (if rs > 1 then " ⎤" else " ]")
          colTexts =
            [((r, c + cc), colspanStr <> pad colspanStr) | cs > 1, cc <- [1 .. cs - 2]]
              <> [ ((r, c + cs - 1), colspanStr <> T.replicate (cellLen - T.length colspanStr - 2) " " <> (if rs > 1 then " ⎤" else " ]"))
                 | cs > 1
                 ]
          rowTexts =
            [ ((r + rr, c + cc), edges cc rr (rowspanStr <> pad rowspanStr))
            | rs > 1,
              cc <- [0 .. cs - 1],
              rr <- [1 .. rs - 1]
            ]
          edges cc rr s =
            let s' = if cc == 0 then (if rr < rs - 1 then "⎢ " else "⎣ ") <> T.drop 2 s else s
             in if cc == cs - 1 then T.dropEnd 2 s' <> (if rr < rs - 1 then " ⎟" else " ⎦") else s'
       in Map.insert (r, c) (startCell' <> pad startCell') (foldl (\m' (k, v) -> Map.insert k v m') m (colTexts <> rowTexts))
    cellText r c = Map.findWithDefault (emptyStr <> pad emptyStr) (r, c) texts
    tshow = T.pack . show
