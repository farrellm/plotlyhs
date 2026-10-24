{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests: each case encodes a value and compares it with a committed
-- file under @test/golden@. Run @cabal test --test-options=--accept@ to
-- regenerate the files after an intended change.
module Main (main) where

import Control.Exception (try)
import Data.Aeson (FromJSON, ToJSON, Value (..), decode, encode, toJSON)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy.Encoding qualified as TLE
import FigureTests (figureTests)
import Graphics.Plotly
import Graphics.Plotly.Blaze qualified as Blaze
import Graphics.Plotly.Html qualified as Html
import Graphics.Plotly.Image
import Graphics.Plotly.Json
import Graphics.Plotly.Lucid qualified as Lucid
import Graphics.Plotly.Version (plotlyJsVersion)
import Lucid qualified
import Optics.Core
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.IO (hClose, openTempFile)
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Text.Blaze.Html.Renderer.Text qualified as Blaze
import TypeErrors (typeErrorTests)

main :: IO ()
main = do
  -- Without a Chrome the image tests are skipped, except on CI.
  chrome <- findChrome
  ci <- lookupEnv "CI"
  defaultMain (tests (imageTests chrome (ci == Just "true")))

tests :: [TestTree] -> TestTree
tests images =
  testGroup
    "plotlyhs"
    [ testGroup "json" jsonTests,
      testGroup "html" htmlTests,
      testGroup "output" outputTests,
      testGroup "images" images,
      testGroup "optics" opticTests,
      testGroup "round trips" roundTrips,
      testGroup "figures" figureTests,
      testGroup "type errors" typeErrorTests
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
            @?= "<script>Plotly.newPlot(\"plot\", {\"data\":[],\"layout\":{\"title\":{\"text\":\"\\u003c/script\\u003e\\u003cb\\u003e\\u0026\"}}});</script>",
    goldenHtml "page" Html.defaultHtmlOptions {Html.divId = Just "plot"},
    goldenHtml "fragment" Html.defaultHtmlOptions {Html.fullHtml = False, Html.responsive = False, Html.divId = Just "plot"},
    goldenHtml "dynamic" Html.defaultHtmlOptions {Html.fullHtml = False, Html.plotlyJs = Html.Dynamic, Html.divId = Just "plot"},
    goldenVsString "lucid-with" "test/golden/lucid-with.html" $
      pure (Lucid.renderBS (Lucid.plotlyWith Html.defaultHtmlOptions {Html.plotlyJs = Html.None, Html.divId = Just "plot"} fig)),
    testCase "line separators are escaped" $
      Html.scriptJson ("a\x2028\&b\x2029\&c" :: T.Text) @?= "\"a\\u2028b\\u2029c\"",
    testCase "the div id is escaped in its attribute" $
      assertBool "unescaped" $
        "id=\"a&quot;&lt;b\"" `T.isInfixOf` Html.toHtml Html.defaultHtmlOptions {Html.divId = Just "a\"<b"} fig,
    testCase "the div id is made from the figure" $ do
      Html.figureDivId fig @?= Html.figureDivId fig
      assertBool "same id" (Html.figureDivId fig /= Html.figureDivId (figure [])),
    testCase "responsive keeps the figure's own setting" $
      assertBool "overridden" $
        "\"responsive\":false" `T.isInfixOf` Html.toHtml Html.defaultHtmlOptions (fig & #config % #responsive ?~ False),
    testCase "the bundle cannot close its script element" $
      assertBool "</script in plotly.min.js" (not ("</script" `T.isInfixOf` T.toLower Html.plotlyJsBundle)),
    testCase "the bundle is the version the library is generated from" $
      assertBool "wrong version" (("plotly.js v" <> plotlyJsVersion) `T.isInfixOf` T.take 200 Html.plotlyJsBundle),
    testCase "an inline page embeds the bundle" $
      assertBool "no bundle" (Html.plotlyJsBundle `T.isInfixOf` Html.toHtml Html.defaultHtmlOptions {Html.plotlyJs = Html.Inline} fig)
  ]

outputTests :: [TestTree]
outputTests =
  [ testCase "toJson and fromJson round trip" $
      fromJson (toJson fig) @?= Right fig,
    testCase "fromJson explains a failure" $
      assertBool "parsed" (either (const True) (const False) (fromJson "[1]")),
    testCase "writeJson and readJson round trip" $ do
      path <- tempPath "plotlyhs.json"
      writeJson path fig
      r <- readJson path
      removeFile path
      r @?= Right fig,
    testCase "writeHtml writes UTF-8" $ do
      path <- tempPath "plotlyhs.html"
      let f = figure [] & #layout % #title % #text ?~ "π"
      Html.writeHtml Html.defaultHtmlOptions path f
      bytes <- BS.readFile path
      removeFile path
      TE.decodeUtf8 bytes @?= Html.toHtml Html.defaultHtmlOptions f,
    testCase "image formats from file names" $
      map imageFormatFromPath ["a.png", "a.JPG", "a.jpeg", "a.webp", "a.svg", "a.pdf", "a.gif"]
        @?= [Just Png, Just Jpeg, Just Jpeg, Just Webp, Just Svg, Just Pdf, Nothing],
    testCase "writeImage needs a format" $ do
      r <- try (writeImage defaultImageOptions "plot.gif" fig)
      case r of
        Left (ImageError _) -> pure ()
        Right () -> assertFailure "wrote an image"
  ]

imageTests :: Maybe FilePath -> Bool -> [TestTree]
imageTests Nothing False = [testCase "skipped: no Chrome" (pure ())]
imageTests Nothing True = [testCase "Chrome" (assertFailure "no Chrome found; CI needs one")]
imageTests (Just _) _ =
  [ testCase "PNG at the given size and scale" $ do
      png <- render Png
      BS.take 8 png @?= "\x89PNG\r\n\x1a\n"
      -- The IHDR chunk: width and height, big-endian.
      (be32 (BS.drop 16 png), be32 (BS.drop 20 png)) @?= (600, 400),
    testCase "JPEG" $ do
      jpeg <- render Jpeg
      BS.take 3 jpeg @?= "\xff\xd8\xff",
    testCase "WebP" $ do
      webp <- render Webp
      (BS.take 4 webp, BS.take 4 (BS.drop 8 webp)) @?= ("RIFF", "WEBP"),
    testCase "SVG" $ do
      svg <- render Svg
      assertBool "not SVG" ("<svg" `BS.isPrefixOf` svg)
      assertBool "not the figure" ("A title" `BS.isInfixOf` svg),
    testCase "PDF" $ do
      pdf <- render Pdf
      BS.take 5 pdf @?= "%PDF-",
    testCase "writeImage takes the format from the file name" $ do
      path <- tempPath "plotlyhs.svg"
      writeImage defaultImageOptions path fig
      svg <- BS.readFile path
      removeFile path
      assertBool "not SVG" ("<svg" `BS.isPrefixOf` svg)
  ]
  where
    render fmt = toImage defaultImageOptions {imageFormat = Just fmt, imageWidth = Just 300, imageHeight = Just 200, imageScale = 2} fig
    be32 = BS.foldl' (\n b -> n * 256 + fromIntegral b) (0 :: Int) . BS.take 4

tempPath :: String -> IO FilePath
tempPath template = do
  tmp <- getTemporaryDirectory
  (path, h) <- openTempFile tmp template
  hClose h
  pure path

goldenHtml :: String -> Html.HtmlOptions -> TestTree
goldenHtml file opts =
  goldenVsString file ("test/golden/" <> file <> ".html") (pure (BL.fromStrict (TE.encodeUtf8 (Html.toHtml opts fig))))

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

fig :: Figure 'NoGrid
fig =
  figure [someTrace scatterTrace, someTrace barTrace]
    & #layout .~ layout'
    & #config % #displayModeBar ?~ DisplayModeBarFalse
    & #frames .~ [emptyObj & #name ?~ "f1" & #data ?~ [someTrace scatterTrace]]
