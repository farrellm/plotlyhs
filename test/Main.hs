{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests: each case encodes a value and compares it with a committed
-- file under @test/golden@. Run @cabal test --test-options=--accept@ to
-- regenerate the files after an intended change.
module Main (main) where

import Data.Aeson (ToJSON, Value, toJSON)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Lazy.Encoding qualified as TLE
import Graphics.Plotly
import Graphics.Plotly.Base qualified as Base
import Graphics.Plotly.Blaze ()
import Graphics.Plotly.Histogram (histMany, histogram)
import Graphics.Plotly.Lucid ()
import Lucid qualified
import Optics.Core
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Text.Blaze.Html.Renderer.Text qualified as Blaze
import Text.Blaze.Html5 qualified as H

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "golden"
    [ testGroup "json" jsonTests,
      testGroup "html" htmlTests
    ]

jsonTests :: [TestTree]
jsonTests =
  [ goldenJson "scatter-markers" scatterMarkers,
    goldenJson "scatter-styled" scatterStyled,
    goldenJson "hbars" hbars',
    goldenJson "pie" pie',
    goldenJson "mesh3d" mesh3d',
    goldenJson "layout" layout',
    goldenJson "plotly" (plotly "div1" [scatterMarkers] & layout .~ layout'),
    goldenJson "simple-scatter" (scatterPlot pairs),
    goldenJson "simple-line" (linePlot pairs),
    goldenJson "simple-hbar" (hbarChart named),
    goldenJson "simple-vbar" (vbarChart named),
    goldenJson "simple-fan" (fanPlot 2 [(1, (2, 0.5)), (2, (3, 0.25))]),
    goldenJson "gog-points" gogPoints,
    goldenJson "gog-line" (line (aes & x .~ fst & y .~ snd) pairs),
    goldenJson "gog-hbox" (hbox (aes & x .~ fst) pairs),
    goldenJson "gog-vbox" (vbox (aes & y .~ snd) pairs),
    goldenJson "gog-hbars" (hbars (aes & x .~ snd & y .~ fst) named),
    goldenJson "gog-hcontour" (hcontour (aes & x .~ fst & y .~ snd & z .~ uncurry (+)) pairs),
    goldenJson "histogram" (histogram 3 [1, 2, 2, 3, 5, 8]),
    goldenJson "hist-many" (histMany 2 [("a", [1, 2, 3]), ("b", [2, 4])])
  ]

htmlTests :: [TestTree]
htmlTests =
  [ goldenVsString "lucid" "test/golden/lucid.html" $
      pure (Lucid.renderBS (Lucid.toHtml plot)),
    goldenVsString "blaze" "test/golden/blaze.html" $
      pure (TLE.encodeUtf8 (Blaze.renderHtml (H.toHtml plot)))
  ]
  where
    plot = plotly "plot" [scatterMarkers] & layout % title ?~ "</script>"

goldenJson :: (ToJSON a) => String -> a -> TestTree
goldenJson file v =
  goldenVsString file ("test/golden/" <> file <> ".json") (pure (prettyJson (toJSON v)))

prettyJson :: Value -> BL.ByteString
prettyJson v = encodePretty' defConfig {confIndent = Spaces 2, confCompare = compare} v <> "\n"

pairs :: [(Double, Double)]
pairs = [(1, 2), (1.5, 3), (2, 3.5)]

named :: [(Text, Double)]
named = [("Simon", 14.5), ("Joe", 18.9), ("Dorothy", 16.2)]

scatterMarkers :: Trace
scatterMarkers =
  scatter
    & Base.x ?~ map (toJSON . fst) pairs
    & Base.y ?~ map (toJSON . snd) pairs
    & mode ?~ [Markers, Lines]
    & name ?~ "points"
    & marker
      ?~ ( defMarker
             & markercolor ?~ catColors ["a", "b", "a" :: Text]
             & Base.size ?~ All (toJSON (12 :: Int))
             & sizeMode ?~ Area
             & symbol ?~ List [Circle, CustomSymbol "star"]
             & markerline ?~ (defMarkerLine & markerlinewidth ?~ All 1)
         )

scatterStyled :: Trace
scatterStyled =
  scatter
    & Base.x ?~ [toJSON (1 :: Int), toJSON (2 :: Int)]
    & Base.line ?~ (defLine & linecolor ?~ ColRGBA 1 2 3 4 & lineshape ?~ Spline & dash ?~ Dashdot)
    & fill ?~ ToNextY
    & hoverinfo ?~ HoverPlus [HoverX, HoverText]
    & hoveron ?~ [HoverPoints, HoverFills]
    & textposition ?~ TopCenter
    & text ?~ ["one", "two"]
    & tracexaxis ?~ "x2"
    & traceshowlegend ?~ Prelude.False

hbars' :: Trace
hbars' =
  bars
    & Base.y ?~ map (toJSON . fst) named
    & Base.x ?~ map (toJSON . snd) named
    & orientation ?~ Horizontal

pie' :: Trace
pie' = pie & values ?~ [toJSON (1 :: Int), toJSON (2 :: Int)] & labels ?~ ["a", "b"] & hole ?~ toJSON (0.4 :: Double)

mesh3d' :: Trace
mesh3d' = mesh3d & i ?~ [0] & j ?~ [1] & k ?~ [2] & tracecolor ?~ ColRGB 10 20 30

layout' :: Layout
layout' =
  defLayout
    & title ?~ "A title"
    & titlefont ?~ (defFont & fontsize ?~ 20 & fontcolor ?~ ColIx 1)
    & xaxis ?~ (defAxis & range ?~ (0, 10) & axistype ?~ Log & axistitle ?~ "x")
    & yaxis2 ?~ (defAxis & domain ?~ (0.5, 1) & tickvals ?~ [toJSON (1 :: Int)] & ticktext ?~ ["one"])
    & zaxis ?~ defAxis
    & barmode ?~ Stack
    & hovermode ?~ Base.False
    & margin ?~ thinMargins
    & annotations ?~ [defAnnotation & annotationtext ?~ "note" & annotataonbgcolor ?~ ColRGB 1 1 1 & annotationalign ?~ AlignLeft]

gogPoints :: Trace
gogPoints =
  points
    ( aes
        & x .~ fst
        & y .~ snd
        & color ?~ (\(a, _) -> RGBA a 0.5 0.25 (0.5 :: Double))
        & size ?~ snd
    )
    pairs
