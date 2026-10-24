{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Grids of subplots, as plotly.py's @plotly.subplots.make_subplots@ makes
-- them. 'makeSubplots' lays out the axes, scenes, polar plots and so on, and
-- 'addTraceAt' puts a trace in a cell:
--
-- @
-- fig :: Figure
-- fig =
--   makeSubplots (subplotOptions 1 2) {subplotTitles = [\"Left\", \"Right\"]}
--     & addTraceAt 1 1 (scatter & #y ?~ dataArray [1, 3, 2 :: Double])
--     & addTraceAt 1 2 (bar & #y ?~ dataArray [2, 1, 3 :: Double])
-- @
--
-- The layout is the same as plotly.py's, so figures look alike in both.
module Graphics.Plotly.Subplots
  ( -- * Making a grid
    makeSubplots,
    SubplotOptions (..),
    subplotOptions,
    SubplotSpec (..),
    spec,
    specFor,
    Sharing (..),
    StartCell (..),

    -- * Placing traces
    addTraceAt,
    addTraceSecondaryY,
    placeTrace,

    -- * The grid
    Grid (..),
    Cell (..),
    SubplotRef (..),
    gridCell,
    printGrid,
  )
where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Plotly.Core
import Graphics.Plotly.Types (SubplotId (..))
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

-- | What goes in a cell.
data SubplotSpec = SubplotSpec
  { -- | The kind of subplot. 'SubplotNone' is not allowed.
    kind :: SubplotKind,
    -- | A second y axis on the right, for an 'SubplotXY' cell.
    secondaryY :: Bool,
    -- | How many columns the cell spans, to the right. The cells it covers
    -- must be 'Nothing' in 'specs'.
    colspan :: Int,
    -- | How many rows the cell spans, downwards (upwards from
    -- 'BottomLeft'). The cells it covers must be 'Nothing' in 'specs'.
    rowspan :: Int,
    -- | Padding inside the cell, as a fraction of the plot's width or
    -- height: left, right, top and bottom.
    padLeft :: Double,
    padRight :: Double,
    padTop :: Double,
    padBottom :: Double
  }
  deriving stock (Eq, Show)

-- | A cell of this kind, spanning one row and column, with no padding.
spec :: SubplotKind -> SubplotSpec
spec k = SubplotSpec k False 1 1 0 0 0 0

-- | A cell for traces of this type: @specFor Pie@ is @spec SubplotDomain@.
specFor :: TraceType -> SubplotSpec
specFor = spec . traceSubplotKind

-- | The grid to make: the arguments of plotly.py's @make_subplots@.
data SubplotOptions = SubplotOptions
  { rows :: Int,
    cols :: Int,
    -- | What goes in each cell, a list per row. 'Nothing' leaves a cell
    -- empty. By default every cell is an 'SubplotXY' subplot.
    specs :: Maybe [[Maybe SubplotSpec]],
    sharedXAxes :: Sharing,
    sharedYAxes :: Sharing,
    startCell :: StartCell,
    -- | The space between columns, as a fraction of the plot's width. By
    -- default 0.2 divided by the columns (0.4 with a secondary y axis).
    horizontalSpacing :: Maybe Double,
    -- | The space between rows, as a fraction of the plot's height. By
    -- default 0.3 divided by the rows (0.5 with subplot titles).
    verticalSpacing :: Maybe Double,
    -- | A title over each cell, in the order of 'specs', skipping empty
    -- cells. An empty title is left out.
    subplotTitles :: [Text],
    -- | Relative widths of the columns. Equal by default.
    columnWidths :: Maybe [Double],
    -- | Relative heights of the rows, from row 1. Equal by default.
    rowHeights :: Maybe [Double],
    -- | A title over each column.
    columnTitles :: [Text],
    -- | A title to the right of each row.
    rowTitles :: [Text],
    -- | A title under the whole grid.
    xTitle :: Maybe Text,
    -- | A title left of the whole grid.
    yTitle :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | A grid with this many rows and columns, of 'SubplotXY' cells, and
-- plotly.py's defaults for everything else.
subplotOptions :: Int -> Int -> SubplotOptions
subplotOptions r c =
  SubplotOptions
    { rows = r,
      cols = c,
      specs = Nothing,
      sharedXAxes = NotShared,
      sharedYAxes = NotShared,
      startCell = TopLeft,
      horizontalSpacing = Nothing,
      verticalSpacing = Nothing,
      subplotTitles = [],
      columnWidths = Nothing,
      rowHeights = Nothing,
      columnTitles = [],
      rowTitles = [],
      xTitle = Nothing,
      yTitle = Nothing
    }

-- | An empty figure with a grid of subplots: its axes, scenes, polar plots
-- and so on, with their domains, and its titles as annotations.
--
-- Calls 'error' when the options are inconsistent, as plotly.py raises:
-- fewer than one row or column, 'specs' of the wrong shape, a span past the
-- edge of the grid, a secondary y axis on a cell that is not 'SubplotXY', a
-- 'SubplotNone' cell, or spacing too large for the grid.
makeSubplots :: SubplotOptions -> Figure
makeSubplots opts =
  (figure [])
    { layout = finalLayout,
      grid = Just (Grid nRows nCols (startCell opts) cellGrid)
    }
  where
    nRows = rows opts
    nCols = cols opts
    bottomUp = startCell opts == BottomLeft
    specGrid
      | nRows < 1 || nCols < 1 = failWith "rows and cols must be at least 1"
      | otherwise = case specs opts of
          Nothing -> replicate nRows (replicate nCols (Just (spec SubplotXY)))
          Just ss
            | length ss == nRows && all ((== nCols) . length) ss -> map (map (fmap checked)) ss
            | otherwise -> failWith ("specs must be " <> show nRows <> " rows of " <> show nCols <> " cells")
    checked s
      | kind s == SubplotNone = failWith "a cell cannot be of kind SubplotNone"
      | secondaryY s && kind s /= SubplotXY = failWith ("a secondary y axis needs a SubplotXY cell, not " <> show (kind s))
      | colspan s < 1 || rowspan s < 1 = failWith "colspan and rowspan must be at least 1"
      | otherwise = s
    hasSecondaryY = or [secondaryY s | row <- specGrid, Just s <- row]
    hSpacing = checkSpacing nCols "horizontal" (fromMaybe ((if hasSecondaryY then 0.4 else 0.2) / fromIntegral nCols) (horizontalSpacing opts))
    vSpacing = checkSpacing nRows "vertical" (fromMaybe ((if null (subplotTitles opts) then 0.3 else 0.5) / fromIntegral nRows) (verticalSpacing opts))
    maxWidth
      | hasSecondaryY = 0.94
      | not (null (rowTitles opts)) = 0.98
      | otherwise = 1.0
    widths = case columnWidths opts of
      Nothing -> replicate nCols ((maxWidth - hSpacing * fromIntegral (nCols - 1)) / fromIntegral nCols)
      Just ws
        | length ws == nCols -> [(maxWidth - hSpacing * fromIntegral (nCols - 1)) * (w / sum ws) | w <- ws]
        | otherwise -> failWith ("columnWidths must have " <> show nCols <> " widths")
    -- Bottom up, whichever corner row 1 is in.
    heights = case rowHeights opts of
      Nothing -> replicate nRows ((1.0 - vSpacing * fromIntegral (nRows - 1)) / fromIntegral nRows)
      Just hs
        | length hs == nRows ->
            (if bottomUp then id else reverse)
              [(1.0 - vSpacing * fromIntegral (nRows - 1)) * (h / sum hs) | h <- hs]
        | otherwise -> failWith ("rowHeights must have " <> show nRows <> " heights")
    -- Where each row and column starts, by row (from row 1) and column.
    colStart c = sum (take c widths) + fromIntegral c * hSpacing
    position r = if bottomUp then r else nRows - 1 - r
    rowStart r = sum (take (position r) heights) + fromIntegral (position r) * vSpacing
    domainOf r c s =
      let c' = c + colspan s - 1
          r' = r + rowspan s - 1
          xs = (colStart c + padLeft s, colStart c' + widths !! c' - padRight s)
          ys
            | bottomUp = (rowStart r + padBottom s, rowStart r' + heights !! r' - padTop s)
            | otherwise = (rowStart r' + padBottom s, rowStart r + heights !! position r - padTop s)
       in if
            | c' >= nCols -> failWith "a colspan reaches past the edge of the grid"
            | r' >= nRows -> failWith "a rowspan reaches past the edge of the grid"
            | otherwise -> (xs, bimap nudge ys)
    nudge y
      | y < 0 && y > -0.01 = 0
      | y > 1 && y < 1.01 = 1
      | y < 0 || y > 1 = failWith "the padding, heights and rows do not fit in the plot"
      | otherwise = y
    -- The cells in order, with their domains.
    placed :: [((Int, Int), SubplotSpec, ((Double, Double), (Double, Double)))]
    placed = [((r, c), s, domainOf r c s) | (r, row) <- zip [0 ..] specGrid, (c, Just s) <- zip [0 ..] row]
    -- Subplots are numbered in cell order, by kind: an x axis once per
    -- cell, and a y axis once more for a secondary y.
    (cellsAt, baseLayout) = foldl initCell (Map.empty, KM.empty) placed
    initCell (refs, lay) (rc, s, (xs, ys)) =
      let xs' = clamp xs
          ys' = clamp ys
          (cell, lay') = case kind s of
            SubplotXY ->
              let x = length [() | c <- Map.elems refs, AxesRef _ _ <- [cellRef c]] + 1
                  y = length [() | c <- Map.elems refs, AxesRef _ _ <- refsOf c] + 1
                  xLabel = axisId "x" x
                  yLabel = axisId "y" y
                  secondary = if secondaryY s then Just (y + 1) else Nothing
                  axes =
                    [ (axisKey "xaxis" x, object ["domain" .= xs', "anchor" .= yLabel]),
                      (axisKey "yaxis" y, object ["domain" .= ys', "anchor" .= xLabel])
                    ]
                      <> [ (axisKey "yaxis" y2, object ["anchor" .= xLabel, "overlaying" .= yLabel, "side" .= ("right" :: Text)])
                         | Just y2 <- [secondary]
                         ]
                  ref = AxesRef (SubplotId xLabel) . SubplotId . axisId "y"
               in ( Cell SubplotXY (rowspan s) (colspan s) (ref y) (ref <$> secondary),
                    foldl (\l (k, v) -> KM.insert k v l) lay axes
                  )
            SubplotDomain -> (Cell SubplotDomain (rowspan s) (colspan s) (DomainRef xs' ys') Nothing, lay)
            k ->
              let name = subplotName k
                  n = count name refs + 1
                  label = axisId name n
               in ( Cell k (rowspan s) (colspan s) (NamedRef (SubplotId label)) Nothing,
                    KM.insert (Key.fromText label) (object ["domain" .= object ["x" .= xs', "y" .= ys']]) lay
                  )
       in (Map.insert rc cell refs, lay')
    count name refs = length [() | c <- Map.elems refs, NamedRef (SubplotId l) <- [cellRef c], T.dropWhileEnd (`elem` ['0' .. '9']) l == name]
    refsOf c = cellRef c : maybe [] pure (cellSecondaryY c)
    cellGrid = [[Map.lookup (r, c) cellsAt | c <- [0 .. nCols - 1]] | r <- [0 .. nRows - 1]]
    -- Shared axes.
    sharedLayout =
      foldl
        (\l (xy, sharing, secondary) -> shareAxes xy sharing secondary l)
        baseLayout
        ([("x", sharedXAxes opts, False), ("y", sharedYAxes opts, False)] <> [("y", sharedYAxes opts, True) | hasSecondaryY])
    shareAxes :: Text -> Sharing -> Bool -> KM.KeyMap Value -> KM.KeyMap Value
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
          cell <- Map.lookup (r, c) cellsAt
          ref <- if secondary then cellSecondaryY cell else Just (cellRef cell)
          let span' = if xy == "x" then cellColspan cell else cellRowspan cell
          case ref of
            AxesRef (SubplotId x) (SubplotId y) | span' == 1 -> Just (if xy == "x" then x else y)
            _ -> Nothing
        groupMatch members lay = snd (foldl step (Nothing, lay) members)
          where
            step (first, l) (r, c, removeLabel) = case axisOf (r, c) of
              Nothing -> (first, l)
              Just ax -> case first of
                Nothing -> (Just ax, l)
                Just f ->
                  let key = axisKeyOf ax
                      update (Object o) =
                        Object
                          ( KM.insert "matches" (String f) o
                              & if removeLabel then KM.insert "showticklabels" (Bool False) else id
                          )
                      update v = v
                   in (first, maybe l (\v -> KM.insert key (update v) l) (KM.lookup key l))
    -- Titles.
    titleAnnotations =
      titlesAt TopEdge 0 (subplotTitles opts) [d | (_, _, d) <- placed]
        <> titlesAt TopEdge 0 (columnTitles opts) (catMaybes [domainAt (if bottomUp then nRows - 1 else 0) c | c <- [0 .. nCols - 1]])
        <> titlesAt RightEdge 0 (rowTitles opts) (catMaybes [domainAt r (nCols - 1) | r <- [0 .. nRows - 1]])
        <> titlesAt BottomEdge 30 (maybe [] pure (xTitle opts)) [((0, maxWidth), (0, 1))]
        <> titlesAt LeftEdge 40 (maybe [] pure (yTitle opts)) [((0, 1), (0, 1))]
    domainAt r c = lookup (r, c) [(rc, d) | (rc, _, d) <- placed]
    finalLayout =
      Obj $
        if null titleAnnotations
          then sharedLayout
          else KM.insert "annotations" (toJSON titleAnnotations) sharedLayout
    checkSpacing n name sp
      | sp < 0 || sp > 1 = failWith (name <> " spacing must be between 0 and 1")
      | n > 1 && sp > 1 / fromIntegral (n - 1) = failWith (name <> " spacing must be at most 1 / " <> show (n - 1) <> " for this grid")
      | otherwise = sp

data Edge = TopEdge | BottomEdge | RightEdge | LeftEdge

-- | Annotations for titles at an edge of each domain, as plotly.py places
-- them, shifted away from it by the offset in pixels. An empty title, or
-- one past the last domain, is left out.
titlesAt :: Edge -> Double -> [Text] -> [((Double, Double), (Double, Double))] -> [Value]
titlesAt edge offset titles domains =
  [ object $
      [ "x" .= x,
        "y" .= y,
        "xref" .= ("paper" :: Text),
        "yref" .= ("paper" :: Text),
        "text" .= t,
        "showarrow" .= False,
        "font" .= object ["size" .= (16 :: Int)],
        "xanchor" .= xanchor,
        "yanchor" .= yanchor
      ]
        <> ["xshift" .= xshift | xshift /= 0]
        <> ["yshift" .= yshift | yshift /= 0]
        <> ["textangle" .= angle | angle /= 0]
  | (t, ((x0, x1), (y0, y1))) <- zip titles domains,
    not (T.null t),
    let (x, y, xanchor, yanchor, xshift, yshift, angle) = case edge of
          TopEdge -> ((x0 + x1) / 2, y1, "center", "bottom", 0, offset, 0 :: Int)
          BottomEdge -> ((x0 + x1) / 2, y0, "center", "top", 0, -offset, 0)
          RightEdge -> (x1, (y0 + y1) / 2, "left", "middle", offset, 0, 90)
          LeftEdge -> (x0, (y0 + y1) / 2, "right", "middle", -offset, 0, -90) :: (Double, Double, Text, Text, Double, Double, Int)
  ]

-- | The layout key of a subplot kind's objects, and its prefix in ids.
subplotName :: SubplotKind -> Text
subplotName = \case
  SubplotScene -> "scene"
  SubplotGeo -> "geo"
  SubplotPolar -> "polar"
  SubplotTernary -> "ternary"
  SubplotMap -> "map"
  SubplotSmith -> "smith"
  k -> error ("Graphics.Plotly.Subplots: no named subplots of kind " <> show k)

-- | The trace attribute that names a subplot of this kind.
subplotAttr :: SubplotKind -> Text
subplotAttr = \case
  SubplotScene -> "scene"
  SubplotGeo -> "geo"
  _ -> "subplot"

-- | @axisId "x" 1@ is @"x"@ and @axisId "x" 2@ is @"x2"@.
axisId :: Text -> Int -> Text
axisId base n
  | n <= 1 = base
  | otherwise = base <> T.pack (show n)

axisKey :: Text -> Int -> KM.Key
axisKey base n = Key.fromText (axisId base n)

-- | The layout key of an axis id: @"x2"@ is @xaxis2@.
axisKeyOf :: Text -> KM.Key
axisKeyOf ax = Key.fromText (T.take 1 ax <> "axis" <> T.drop 1 ax)

clamp :: (Double, Double) -> (Double, Double)
clamp (lo, hi) = (max 0 lo, min 1 hi)

bimap :: (a -> b) -> (a, a) -> (b, b)
bimap f (a, b) = (f a, f b)

failWith :: String -> a
failWith msg = error ("Graphics.Plotly.Subplots.makeSubplots: " <> msg)

-- | Add a trace to the cell at a row and column, counted from 1, setting
-- the attributes that place it there: @xaxis@ and @yaxis@, @scene@, @geo@,
-- @subplot@ or @domain@.
--
-- Calls 'error', as plotly.py raises, if the figure has no grid, the cell
-- is outside it or empty, or the trace cannot be drawn on the cell's kind
-- of subplot.
addTraceAt :: Int -> Int -> Trace t -> Figure -> Figure
addTraceAt r c tr f = f & #data %~ (<> [placeTrace False r c f (someTrace tr)])

-- | Add a trace to the secondary y axis of the cell at a row and column.
-- Calls 'error' as 'addTraceAt' does, and if the cell has no secondary y
-- axis.
addTraceSecondaryY :: Int -> Int -> Trace t -> Figure -> Figure
addTraceSecondaryY r c tr f = f & #data %~ (<> [placeTrace True r c f (someTrace tr)])

-- | Place a trace in a cell of the figure's grid, on its secondary y axis
-- if 'True', without adding it. Calls 'error' as 'addTraceAt' does.
placeTrace :: Bool -> Int -> Int -> Figure -> SomeTrace -> SomeTrace
placeTrace secondary r c f tr = case grid f of
  Nothing -> failure "the figure has no subplot grid; make it with makeSubplots"
  Just g
    | r < 1 || c < 1 || r > gridRows g || c > gridCols g ->
        failure ("(" <> show r <> ", " <> show c <> ") is outside the " <> show (gridRows g) <> " by " <> show (gridCols g) <> " grid")
    | otherwise -> case gridCell r c g of
        Nothing -> failure ("there is no subplot at (" <> show r <> ", " <> show c <> ")")
        Just cell -> do
          let ref
                | secondary = fromMaybe (failure ("the subplot at (" <> show r <> ", " <> show c <> ") has no secondary y axis")) (cellSecondaryY cell)
                | otherwise = cellRef cell
              trKind = traceSubplotKind <$> someTraceType tr
          if trKind /= Just (cellKind cell)
            then failure ("a " <> traceName tr <> " trace cannot go on the " <> show (cellKind cell) <> " subplot at (" <> show r <> ", " <> show c <> ")")
            else case ref of
              AxesRef x y -> tr & attr "xaxis" ?~ toJSON x & attr "yaxis" ?~ toJSON y
              NamedRef n -> tr & attr (subplotAttr (cellKind cell)) ?~ toJSON n
              DomainRef xs ys -> tr & attr "domain.x" ?~ toJSON xs & attr "domain.y" ?~ toJSON ys
  where
    failure msg = error ("Graphics.Plotly.Subplots.addTraceAt: " <> msg)
    traceName (SomeTrace o) = case KM.lookup "type" o of
      Just (String t) -> T.unpack t
      _ -> "scatter"

-- | The grid of a figure, drawn as plotly.py's @print_grid@ draws it:
-- each cell with its row, column and subplots, or @(empty)@.
--
-- @
-- This is the format of your plot grid:
-- [ (1,1) x,y   ]  [ (1,2) x2,y2 ]
-- @
--
-- 'Nothing' if the figure has no grid.
printGrid :: Figure -> Maybe Text
printGrid f = gridText <$> grid f

gridText :: Grid -> Text
gridText g =
  "This is the format of your plot grid:\n"
    <> T.concat [T.intercalate "  " [cellText r c | c <- [0 .. nCols - 1]] <> "\n" | r <- printOrder]
  where
    nRows = gridRows g
    nCols = gridCols g
    -- Top to bottom on the page.
    printOrder = if gridStartCell g == BottomLeft then [nRows - 1, nRows - 2 .. 0] else [0 .. nRows - 1]
    cells = [((r, c), cell) | (r, row) <- zip [0 ..] (gridCells g), (c, Just cell) <- zip [0 ..] row]
    label r c cell =
      let keys = sort (concatMap layoutKeys (cellRef cell : maybe [] pure (cellSecondaryY cell)))
       in "(" <> tshow (r + 1) <> "," <> tshow (c + 1) <> ") " <> T.replace "axis" "" (T.intercalate "," (dedupe keys))
    layoutKeys = \case
      AxesRef (SubplotId x) (SubplotId y) -> [Key.toText (axisKeyOf x), Key.toText (axisKeyOf y)]
      NamedRef (SubplotId n) -> [n]
      DomainRef _ _ -> []
    dedupe = \case
      a : b : rest | a == b -> dedupe (b : rest)
      a : rest -> a : dedupe rest
      [] -> []
    cellLen = maximum (0 : [T.length (label r c cell) | ((r, c), cell) <- cells]) + 4
    pad s = T.replicate (cellLen - T.length s) " "
    colspanStr = "       -"
    rowspanStr = "       :"
    emptyStr = "    (empty) "
    -- Each cell's text, as plotly.py fills them in, in order: a spanning
    -- cell writes over the cells it covers.
    texts = foldl place Map.empty cells
    place m ((r, c), cell) =
      let rs = cellRowspan cell
          cs = cellColspan cell
          start = (if rs > 1 then "⎡ " else "[ ") <> label r c cell
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
