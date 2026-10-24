{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tests of the figure helpers: subplots, selectors, shapes, animation,
-- templates and colours. The subplot goldens match plotly.py's output for
-- the same calls (see @scripts/check-figures.py@).
module FigureTests (figureTests, subplotFigures) where

import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (isJust)
import Data.Text qualified as T
import Graphics.Plotly
import Graphics.Plotly.Colors
import Graphics.Plotly.Colors.Qualitative qualified as Qualitative
import Graphics.Plotly.Colors.Sequential qualified as Sequential
import Graphics.Plotly.Template qualified as Template
import Optics.Core
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit

figureTests :: [TestTree]
figureTests =
  [ testGroup "subplots" (map goldenFigure subplotFigures <> gridTests <> placementTests),
    testGroup "selectors" selectorTests,
    testGroup "axes" axisTests,
    testGroup "shapes" (map goldenFigure shapeFigures <> shapeTests),
    testGroup "animation" [goldenFigure ("animation", animated)],
    testGroup "templates" templateTests,
    testGroup "colours" colourTests
  ]

goldenFigure :: (String, Figure 'NoGrid) -> TestTree
goldenFigure (name, f) =
  goldenVsString name ("test/golden/figures/" <> name <> ".json") (pure (prettyJson (toJSON f)))

prettyJson :: Value -> BL.ByteString
prettyJson v = encodePretty' defConfig {confIndent = Spaces 2, confCompare = compare} v <> "\n"

ys :: [Double] -> Trace Scatter
ys vs = scatter & #y ?~ dataArray vs

-- | Figures made with makeSubplots, by name. scripts/check-figures.py makes
-- each with plotly.py.
subplotFigures :: [(String, Figure 'NoGrid)]
subplotFigures =
  [ ("subplots-basic", forgetGrid (makeSubplots @(Uniform 2 2 XY) subplotOptions)),
    ("subplots-shared-x", forgetGrid (makeSubplots @(Uniform 3 2 XY) subplotOptions {sharedXAxes = ShareColumns})),
    ("subplots-shared-y", forgetGrid (makeSubplots @(Uniform 2 3 XY) subplotOptions {sharedYAxes = ShareRows})),
    ("subplots-shared-all", forgetGrid (makeSubplots @(Uniform 2 2 XY) subplotOptions {sharedXAxes = ShareAll, sharedYAxes = ShareAll})),
    ("subplots-shared-bottom-left", forgetGrid (makeSubplots @(Uniform 3 2 XY) subplotOptions {sharedXAxes = ShareColumns, sharedYAxes = ShareAll, startCell = BottomLeft})),
    ( "subplots-secondary-y",
      forgetGrid $
        makeSubplots @'[ '[PlotY2, XY], '[XY, PlotY2]] subplotOptions {sharedYAxes = ShareAll}
          & addTraceAt @1 @1 (ys [1, 2])
          & addTraceSecondaryY @1 @1 (ys [20, 10])
          & addTraceSecondaryY @2 @2 (ys [3, 4])
    ),
    ( "subplots-mixed",
      forgetGrid $
        makeSubplots @'[ '[XY, Polar, Domain], '[Scene, Geo, Ternary]] subplotOptions {subplotTitles = ["xy", "polar", "", "scene"]}
          & addTraceAt @1 @1 (ys [1, 3, 2])
          & addTraceAt @1 @2 (scatterpolar & #r ?~ dataArray [1, 2 :: Double])
          & addTraceAt @1 @3 (pie & #values ?~ dataArray [1, 2 :: Double])
          & addTraceAt @2 @1 (scatter3d & #z ?~ dataArray [1, 2 :: Double])
          & addTraceAt @2 @2 (scattergeo & #lat ?~ dataArray [1, 2 :: Double])
          & addTraceAt @2 @3 (scatterternary & #a ?~ dataArray [1, 2 :: Double])
    ),
    ( "subplots-spans",
      forgetGrid $
        makeSubplots
          @'[ '[ 'Span 1 2 XY, 'Blank, 'Span 3 1 XY],
              '[ 'Span 2 1 XY, XY, 'Blank],
              '[ 'Blank, 'Blank, 'Blank]
            ]
          subplotOptions
            { cellPadding = \r c -> if (r, c) == (2, 1) then noPadding {padLeft = 0.02, padTop = 0.01} else noPadding,
              sharedXAxes = ShareColumns,
              startCell = BottomLeft
            }
    ),
    ( "subplots-titles",
      forgetGrid $
        makeSubplots @(Uniform 2 2 XY)
          subplotOptions
            { subplotTitles = ["a", "b", "c", "d"],
              columnTitles = ["left", "right"],
              rowTitles = ["top", "bottom"],
              xTitle = Just "x",
              yTitle = Just "y",
              columnWidths = Just fromIntegral,
              rowHeights = Just (\r -> fromIntegral (3 - r)),
              horizontalSpacing = Just 0.1
            }
    ),
    ( "subplots-heights-bottom-left",
      forgetGrid $
        makeSubplots @(Uniform 2 1 XY)
          subplotOptions {rowHeights = Just (\r -> if r == 1 then 3 else 1), startCell = BottomLeft, verticalSpacing = Just 0.05, columnTitles = ["only"]}
    )
  ]

gridTests :: [TestTree]
gridTests =
  [ testCase "printGrid" $
      printGrid (makeSubplots @(Uniform 2 2 XY) subplotOptions)
        @?= T.unlines
          [ "This is the format of your plot grid:",
            "[ (1,1) x,y   ]  [ (1,2) x2,y2 ]",
            "[ (2,1) x3,y3 ]  [ (2,2) x4,y4 ]"
          ],
    testCase "printGrid with spans, empty cells and a secondary y" $
      printGrid (makeSubplots @'[ '[ 'Span 1 2 PlotY2, 'Blank], '[ 'Span 2 1 Scene, Domain], '[ 'Blank, 'Blank]] subplotOptions)
        @?= T.unlines
          [ "This is the format of your plot grid:",
            "[ (1,1) x,y,y2           -       ]",
            "⎡ (2,1) scene  ⎤  [ (2,2)        ]",
            "⎣      :       ⎦      (empty)     "
          ],
    testCase "too much spacing is clamped to what fits" $
      layout (makeSubplots @(Uniform 3 1 XY) subplotOptions {verticalSpacing = Just 0.9})
        @?= layout (makeSubplots @(Uniform 3 1 XY) subplotOptions {verticalSpacing = Just 0.5}),
    testCase "zero widths are equal widths" $
      layout (makeSubplots @(Uniform 1 3 XY) subplotOptions {columnWidths = Just (const 0)})
        @?= layout (makeSubplots @(Uniform 1 3 XY) subplotOptions),
    testCase "padding that does not fit is clamped" $
      let f = makeSubplots @(Uniform 1 1 XY) subplotOptions {cellPadding = \_ _ -> Padding 0 0 (-1) (-1)}
       in layout f ^. attr "yaxis.domain" @?= Just (toJSON [0, 1 :: Double])
  ]
  where
    layout :: Figure g -> Layout
    layout f = f ^. #layout

placementTests :: [TestTree]
placementTests =
  [ testCase "addTraceAt sets the axes" $
      refs (makeSubplots @(Uniform 2 2 XY) subplotOptions & addTraceAt @2 @1 (ys [1]))
        @?= [(Just "x3", Just "y3")],
    testCase "addTraceIn with cells from withUniformSubplots" $
      withUniformSubplots 3 1 subplotOptions (\f cells -> refs (foldl (\f' ix' -> addTraceIn ix' (ys [1]) f') f (concat cells)))
        @?= [(Just "x", Just "y"), (Just "x2", Just "y2"), (Just "x3", Just "y3")],
    testCase "withUniformSubplots lays out as makeSubplots" $
      withUniformSubplots 2 3 subplotOptions (\f _ -> toJSON f) @?= toJSON (makeSubplots @(Uniform 2 3 XY) subplotOptions),
    testCase "withUniformSubplots has at least one cell" $
      withUniformSubplots 0 (-1) subplotOptions (\_ cells -> map (map cellPosition) cells) @?= [[(1, 1)]],
    testCase "cell and cellY2 name their cells" $
      (cellPosition (cell @2 @1 @'[ '[XY], '[PlotY2]]), cellPosition (cellY2 @2 @1 @'[ '[XY], '[PlotY2]])) @?= ((2, 1), (2, 1))
  ]
  where
    refs f = [(t ^. attr "xaxis", t ^. attr "yaxis") | t <- f ^. #data]

-- | A 2 by 2 grid with a scatter and a bar trace in each column.
grid2 :: Figure ('Grid '[ '[PlotY2, XY], '[XY, XY]])
grid2 =
  makeSubplots subplotOptions
    & addTraceAt @1 @1 (ys [1] & #name ?~ "a")
    & addTraceSecondaryY @1 @1 (ys [2] & #name ?~ "b")
    & addTraceAt @1 @2 (bar & #name ?~ "c")
    & addTraceAt @2 @1 (bar & #name ?~ "d")
    & addTraceAt @2 @2 (ys [3] & #name ?~ "e")

names :: Selector g -> Figure g -> [Maybe Value]
names sel f = map (^. attr "name") (selectTraces sel f)

selectorTests :: [TestTree]
selectorTests =
  [ testCase "mempty selects every trace" $
      length (selectTraces mempty grid2) @?= 5,
    testCase "by type" $
      names (ofType @Bar) grid2 @?= [Just "c", Just "d"],
    testCase "by cell" $
      names (inCells (atCell @1 @1)) grid2 @?= [Just "a", Just "b"],
    testCase "by row" $
      names (inCells (inRow @2)) grid2 @?= [Just "d", Just "e"],
    testCase "by column and type" $
      names (inCells (inCol @2) <> ofType @Scatter) grid2 @?= [Just "e"],
    testCase "by cell index" $
      names (inCells (cellsOf (cell @2 @2))) grid2 @?= [Just "e"],
    testCase "on a secondary y axis" $
      names onSecondaryY grid2 @?= [Just "b"],
    testCase "by index and name" $
      (names (atIndex 3) grid2, names (named "e") grid2) @?= ([Just "d"], [Just "e"]),
    testCase "updateTracesOf updates only its type, typed" $
      [t ^. attr "opacity" | t <- (grid2 & updateTracesOf @Bar mempty (#opacity ?~ 0.5)) ^. #data]
        @?= [Nothing, Nothing, Just (Number 0.5), Just (Number 0.5), Nothing],
    testCase "tracesOf: setting what is read changes nothing" $
      (grid2 & tracesOf @Scatter mempty %~ id) @?= grid2,
    testCase "tracesOf: setting twice is setting once" $
      let set1 = tracesOf @Scatter mempty % #opacity ?~ 0.1
          set2 = tracesOf @Scatter mempty % #opacity ?~ 0.2
       in (grid2 & set1 & set2) @?= (grid2 & set2),
    testCase "forEachTrace runs in order" $
      fst (forEachTrace (ofType @Scatter) (\t -> ([t ^. attr "name"], t)) grid2) @?= [Just "a", Just "b", Just "e"]
  ]

axisTests :: [TestTree]
axisTests =
  [ testCase "updateXAxes in a row" $
      shown (grid2 & updateXAxes (inRow @1) (#showgrid ?~ False))
        @?= [("xaxis", Just False), ("xaxis2", Just False), ("xaxis3", Nothing), ("xaxis4", Nothing)],
    testCase "updateXAxes everywhere" $
      shown (grid2 & updateXAxes allCells (#showgrid ?~ False))
        @?= [(k, Just False) | k <- ["xaxis", "xaxis2", "xaxis3", "xaxis4"]],
    testCase "updateYAxes takes secondary axes" $
      [(k, l ^. attr (k <> ".showgrid")) | let l = (grid2 & updateYAxes (atCell @1 @1) (#showgrid ?~ False)) ^. #layout, k <- ["yaxis", "yaxis2", "yaxis3"]]
        @?= [("yaxis", Just (Bool False)), ("yaxis2", Just (Bool False)), ("yaxis3", Nothing)],
    testCase "without a grid there is one axis to update" $
      (figure [] & updateXAxes allCells (#showgrid ?~ False)) ^. #layout @?= (emptyObj & #xaxis % #showgrid ?~ False)
  ]
  where
    shown f = [(k, (f ^. #layout % attr (k <> ".showgrid")) >>= \case Bool b -> Just b; _ -> Nothing) | k <- ["xaxis", "xaxis2", "xaxis3", "xaxis4"]]

-- | Shapes across subplots, as plotly.py's add_hline and so on draw them.
shapeFigures :: [(String, Figure 'NoGrid)]
shapeFigures =
  [ ( "shapes-grid",
      forgetGrid $
        makeSubplots @(Uniform 2 2 XY) subplotOptions
          & addTraceAt @1 @1 (ys [1, 2])
          & addTraceAt @2 @2 (ys [2, 1])
          & addHLine defaultSpanOptions {annotation = Just (#text ?~ "limit")} (1.5 :: Double)
          & addVRect defaultSpanOptions {annotation = Just (#text ?~ "zone"), annotationPosition = AtTop, annotationOutside = True, shapeStyle = #fillcolor ?~ "green"} (0 :: Double) 1
          & addVLine defaultSpanOptions {cells = inCol @2, excludeEmptySubplots = False, annotation = Just (#text ?~ "now"), annotationPosition = AtBottomLeft} (0.5 :: Double)
    ),
    ( "shapes-single",
      figure [someTrace (ys [1, 2])]
        & addHRect defaultSpanOptions {annotation = Just (#text ?~ "band"), annotationPosition = AtLeft} (1 :: Double) 2
        & addVLine defaultSpanOptions {annotation = Just (#text ?~ "day")} ("2024-01-02" :: T.Text)
        & addVRect defaultSpanOptions {annotation = Just (#text ?~ "week"), annotationPosition = AtTop} ("2024-01-01" :: T.Text) "2024-01-08"
    )
  ]

shapeTests :: [TestTree]
shapeTests =
  [ testCase "addShapeAt keeps a domain reference" $
      [(s ^. attr "xref", s ^. attr "yref") | s <- shapesOf (grid2 & addShapeAt (atCell @2 @2) (emptyObj & attr "xref" ?~ "x domain"))]
        @?= [(Just "x4 domain", Just "y5")],
    testCase "addAnnotationAt keeps a yref to another axis" $
      [(a ^. attr "xref", a ^. attr "yref") | a <- annotationsOf (grid2 & addAnnotationAt (atCell @1 @2) (emptyObj & attr "yref" ?~ "y3"))]
        @?= [(Just "x2", Just "y3")],
    testCase "addShape appends" $
      length (shapesOf (figure [] & addShape emptyObj & addShape emptyObj)) @?= 2
  ]
  where
    shapesOf f = view (#layout % #shapes) f ^. non []
    annotationsOf f = view (#layout % #annotations) f ^. non []

animated :: Figure 'NoGrid
animated =
  figure [someTrace (ys [1, 2])]
    & addFrames frames'
    & #layout % #updatemenus ?~ [playPauseMenu (emptyObj & #frame % #duration ?~ 500 & #transition % #duration ?~ 300)]
    & #layout % #sliders ?~ [framesSlider (emptyObj & #frame % #duration ?~ 0) frames']
  where
    frames' = [frame (T.pack (show i)) [someTrace (ys [i, i + 1])] | i <- [1, 2, 3 :: Double]]

templateTests :: [TestTree]
templateTests =
  [ testCase "every template decodes" $
      mapM_ (\n -> assertBool (T.unpack n) (isJust (Template.named n) && Template.named n /= Just mempty)) Template.names,
    testCase "named merges with +" $
      Template.named "plotly_dark+presentation" @?= Just (Template.plotlyDark <> Template.presentation),
    testCase "an unknown name" $
      Template.named "plotly+nope" @?= Nothing,
    testCase "trace templates cycle to a common length" $
      toJSON (traces' [colour "a", colour "b"] <> traces' [width 1, width 2, width 3])
        @?= object
          [ "data"
              .= object
                [ "scatter"
                    .= [ object ["line" .= object ["color" .= c, "width" .= w]]
                       | (c, w) <- zip (cycle ["a", "b" :: T.Text]) [1, 2, 3, 1, 2, 3 :: Int]
                       ]
                ]
          ],
    testCase "layout attributes merge deeply" $
      toJSON (layoutTemplate (object ["font" .= object ["size" .= (1 :: Int), "color" .= ("red" :: T.Text)]]) <> layoutTemplate (object ["font" .= object ["size" .= (2 :: Int)]]))
        @?= object ["layout" .= object ["font" .= object ["size" .= (2 :: Int), "color" .= ("red" :: T.Text)]]],
    testCase "withTemplate sets the layout's template" $
      (figure [] & Template.withTemplate Template.gridon) ^. #layout % #template @?= Just Template.gridon,
    goldenFigure ("template", figure [someTrace (ys [1])] & Template.withTemplate (Template.ggplot2 <> Template.xgridoff))
  ]
  where
    width :: Int -> Value
    width w = object ["line" .= object ["width" .= w]]
    colour :: T.Text -> Value
    colour c = object ["line" .= object ["color" .= c]]
    traces' ts = Template (fromObject' (object ["data" .= object ["scatter" .= ts]]))
    layoutTemplate l = Template (fromObject' (object ["layout" .= l]))
    fromObject' = \case
      Object o -> o
      _ -> mempty

colourTests :: [TestTree]
colourTests =
  [ testCase "plotly.py's default sequence" $
      take 3 Qualitative.plotly @?= ["#636EFA", "#EF553B", "#00CC96"],
    testCase "toColorscale spaces stops evenly" $
      toColorscale ["#000000", "#808080", "#ffffff"] @?= ColorscaleStops [(0, "#000000"), (0.5, "#808080"), (1, "#ffffff")],
    testCase "namedColorscale ignores case and reverses _r" $
      (namedColorscale "VIRIDIS", namedColorscale "viridis_r") @?= (Just Sequential.viridis, Just (reverse Sequential.viridis)),
    testCase "sampleColorscale interpolates" $
      sampleColorscale (ColorscaleStops [(0, "#000000"), (1, "rgb(200, 100, 50)")]) [0, 0.5, 1, 2]
        @?= Just [rgb 0 0 0, rgb 100 50 25, rgb 200 100 50, rgb 200 100 50],
    testCase "sampleColorscale by name" $
      sampleColorscale "Viridis" [0, 1] @?= Just [rgb 0x44 0x01 0x54, rgb 0xfd 0xe7 0x25],
    testCase "sampleColorscale keeps opacity" $
      sampleColorscale (ColorscaleStops [(0, "rgba(0, 0, 0, 0)"), (1, "#fff")]) [0.5] @?= Just [rgba 128 128 128 0.5],
    testCase "sampleColorscale needs colours it can read" $
      (sampleColorscale (ColorscaleStops [(0, "red"), (1, "blue")]) [0.5], sampleColorscale "nope" [0]) @?= (Nothing, Nothing),
    testCase "nColors" $
      nColors "#000" "#fff" 3 @?= Just [rgb 0 0 0, rgb 128 128 128, rgb 255 255 255]
  ]
