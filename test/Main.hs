{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests: each case encodes a value and compares it with a committed
-- file under @test/golden@. Run @cabal test --test-options=--accept@ to
-- regenerate the files after an intended change.
module Main (main) where

import Data.Aeson (FromJSON, ToJSON, Value (..), decode, encode, toJSON)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.ByteString.Lazy qualified as BL
import Data.Text.Lazy.Encoding qualified as TLE
import Graphics.Plotly
import Graphics.Plotly.Blaze qualified as Blaze
import Graphics.Plotly.Lucid qualified as Lucid
import Lucid qualified
import Optics.Core
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Text.Blaze.Html.Renderer.Text qualified as Blaze

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "plotlyhs"
    [ testGroup "json" jsonTests,
      testGroup "html" htmlTests,
      testGroup "optics" opticTests,
      testGroup "round trips" roundTrips
    ]

jsonTests :: [TestTree]
jsonTests =
  [ goldenJson "scatter" scatterTrace,
    goldenJson "bar" barTrace,
    goldenJson "heatmap" heatmapTrace,
    goldenJson "layout" layout',
    goldenJson "figure" fig,
    goldenJson "raw" (rawTrace "future" & attr "marker.line.width" ?~ Number 2)
  ]

htmlTests :: [TestTree]
htmlTests =
  [ goldenVsString "lucid" "test/golden/lucid.html" $
      pure (Lucid.renderBS (Lucid.plotlyCDN >> Lucid.plotly "plot" fig)),
    goldenVsString "blaze" "test/golden/blaze.html" $
      pure (TLE.encodeUtf8 (Blaze.renderHtml (Blaze.plotlyCDN >> Blaze.plotly "plot" fig))),
    testCase "a string cannot close the script" $
      let hostile = figure [] & #layout % #title % #text ?~ "</script><b>&"
       in Lucid.renderBS (Lucid.plotlyJS "plot" hostile)
            @?= "<script>Plotly.newPlot(\"plot\", {\"data\":[],\"layout\":{\"title\":{\"text\":\"\\u003c/script\\u003e\\u003cb\\u003e\\u0026\"}}});</script>"
  ]

opticTests :: [TestTree]
opticTests =
  [ testCase "an unset attribute reads as Nothing" $
      scatter ^. #opacity @?= Nothing,
    testCase "a set attribute reads back" $
      (scatter & #opacity ?~ 0.5) ^. #opacity @?= Just 0.5,
    testCase "unsetting an attribute removes it" $
      (scatter & #opacity ?~ 0.5 & #opacity .~ Nothing) @?= scatter,
    testCase "an unset object reads as empty" $
      scatter ^. #marker @?= emptyObj,
    testCase "emptying an object removes it" $
      (scatter & #marker % #size ?~ One 3 & #marker % #size .~ Nothing) @?= scatter,
    testCase "attr reads a typed attribute" $
      (scatter & #marker % #line % #width ?~ One 2) ^. attr "marker.line.width" @?= Just (Number 2),
    testCase "attr unsets, removing the emptied objects" $
      (scatter & #marker % #line % #width ?~ One 2 & attr "marker.line.width" .~ Nothing) @?= scatter,
    testCase "a numbered subplot" $
      toJSON (emptyObj & xaxisN 2 % #title % #text ?~ "second" :: Layout)
        @?= toJSON (emptyObj & attr "xaxis2.title.text" ?~ "second" :: Layout),
    testCase "subplot 1 has no number" $
      (emptyObj & xaxisN 1 % #visible ?~ True :: Layout) @?= (emptyObj & #xaxis % #visible ?~ True),
    testCase "fromSomeTrace recovers a trace of the right type" $
      fromSomeTrace (someTrace scatterTrace) @?= Just scatterTrace,
    testCase "fromSomeTrace rejects a trace of another type" $
      (fromSomeTrace (someTrace scatterTrace) :: Maybe (Trace Bar)) @?= Nothing,
    testCase "a trace without a type is a scatter trace" $
      someTraceType (SomeTrace mempty) @?= Just Scatter,
    testCase "trace type names" $
      map traceTypeName [Scatter, Scatter3d, Histogram2dcontour] @?= ["scatter", "scatter3d", "histogram2dcontour"],
    testCase "a small opacity has no exponent" $
      rgba 0 0 0 0.05 @?= "rgba(0, 0, 0, 0.05)",
    testCase "flags" $
      encode (Flags (ModeLines :| [ModeMarkers]) :: Flags Mode ModeExtra) @?= "\"lines+markers\"",
    testCase "a lone flag" $
      encode (Flags (ModeLines :| []) :: Flags Mode ModeExtra) @?= "\"lines\"",
    testCase "an extra" $
      encode (Extra ModeNone :: Flags Mode ModeExtra) @?= "\"none\"",
    testCase "a boolean extra" $
      encode (Extra True :: Flags Automargin Bool) @?= "true",
    testCase "an extra cannot combine with flags" $
      (decode "\"lines+none\"" :: Maybe (Flags Mode ModeExtra)) @?= Nothing,
    testCase "an empty string is not a flag list" $
      (decode "\"\"" :: Maybe (Flags Mode ModeExtra)) @?= Nothing,
    testCase "a boolean in an enumeration" $
      encode [VisibleFalse, VisibleLegendonly] @?= "[false,\"legendonly\"]",
    testCase "a subplot id in an enumeration" $
      encode [AnnotationXrefPaper, AnnotationXrefSubplot "x2"] @?= "[\"paper\",\"x2\"]"
  ]

roundTrips :: [TestTree]
roundTrips =
  [ roundTrip "figure" fig,
    roundTrip "trace" scatterTrace,
    roundTrip "flags" (Flags (HoverinfoX :| [HoverinfoY, HoverinfoName]) :: Flags Hoverinfo HoverinfoExtra),
    roundTrip "extras" [Extra HoverinfoAll, Extra HoverinfoSkip, Flags (HoverinfoText :| []) :: Flags Hoverinfo HoverinfoExtra],
    roundTrip "boolean extras" [Extra True, Extra False, Flags (AutomarginHeight :| [AutomarginLeft]) :: Flags Automargin Bool],
    roundTrip "enumeration" [VisibleTrue, VisibleFalse, VisibleLegendonly],
    roundTrip "enumeration with a subplot id" [AnnotationXrefPaper, AnnotationXrefSubplot "x2"],
    roundTrip "colours" [rgb 1 2 3, rgba 4 5 6 0.5, "red", ColorNumber 2],
    roundTrip "colour scales" ["Viridis", ColorscaleStops [(0, "red"), (1, "blue")]]
  ]

roundTrip :: (Eq a, Show a, ToJSON a, FromJSON a) => String -> a -> TestTree
roundTrip name a = testCase name (decode (encode a) @?= Just a)

goldenJson :: (ToJSON a) => String -> a -> TestTree
goldenJson file v =
  goldenVsString file ("test/golden/" <> file <> ".json") (pure (prettyJson (toJSON v)))

prettyJson :: Value -> BL.ByteString
prettyJson v = encodePretty' defConfig {confIndent = Spaces 2, confCompare = compare} v <> "\n"

scatterTrace :: Trace Scatter
scatterTrace =
  scatter
    & #x ?~ dataArray [1, 1.5, 2 :: Double]
    & #y ?~ dataArray [2, 3, 3.5 :: Double]
    & #mode ?~ Flags (ModeLines :| [ModeMarkers])
    & #name ?~ "points"
    & #visible ?~ VisibleLegendonly
    & #text ?~ Many ["one", "two", "three"]
    & #hoverinfo ?~ One (Flags (HoverinfoX :| [HoverinfoText]))
    & #fill ?~ ScatterFillTonexty
    & #xaxis ?~ "x2"
    & #marker % #color ?~ Many ["red", rgb 0 128 255, rgba 0 0 0 0.5]
    & #marker % #size ?~ One 12
    & #marker % #symbol ?~ One SymbolCircleOpen
    & #marker % #line % #width ?~ One 1
    & #line % #shape ?~ ScatterLineShapeSpline
    & #line % #dash ?~ "dashdot"

barTrace :: Trace Bar
barTrace =
  bar
    & #x ?~ dataArray [14.5, 18.9, 16.2 :: Double]
    & #y ?~ dataArray ["Simon", "Joe", "Dorothy" :: String]
    & #orientation ?~ OrientationH
    & #marker % #colorscale ?~ "Viridis"
    & #marker % #colorbar % #title % #text ?~ "score"

heatmapTrace :: Trace Heatmap
heatmapTrace =
  heatmap
    & #z ?~ dataArray [[1, 2], [3, 4 :: Int]]
    & #colorscale ?~ ColorscaleStops [(0, "white"), (1, "black")]
    & #zsmooth ?~ ZsmoothBest

layout' :: Layout
layout' =
  emptyObj
    & #title % #text ?~ "A title"
    & #title % #font % #size ?~ 20
    & #xaxis % #range ?~ (Number 0, Number 10)
    & #xaxis % #type ?~ XaxisTypeLog
    & #xaxis % #title % #text ?~ "x"
    & yaxisN 2 % #domain ?~ (0.5, 1)
    & yaxisN 2 % #overlaying ?~ AnchorSubplot "y"
    & #barmode ?~ LayoutBarmodeStack
    & #hovermode ?~ LayoutHovermodeFalse
    & #margin % #t ?~ 10
    & #scene % #zaxis % #title % #text ?~ "z"
    & #annotations ?~ [emptyObj & #text ?~ "note" & #bgcolor ?~ "white" & #align ?~ XanchorLeft]

fig :: Figure
fig =
  figure [someTrace scatterTrace, someTrace barTrace]
    & #layout .~ layout'
    & #config % #displayModeBar ?~ DisplayModeBarFalse
    & #frames .~ [emptyObj & #name ?~ "f1" & #data ?~ [someTrace scatterTrace]]
