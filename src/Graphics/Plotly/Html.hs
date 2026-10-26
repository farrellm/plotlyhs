{-# LANGUAGE OverloadedStrings #-}

-- |
-- Render figures to HTML: a page of its own, or a fragment to put in one.
--
-- @
-- writeHtml defaultHtmlOptions "plot.html" fig
-- showInBrowser fig
-- @
module Graphics.Plotly.Html
  ( -- * Options
    HtmlOptions (..),
    defaultHtmlOptions,
    IncludePlotlyJs (..),

    -- * Rendering
    toHtml,
    writeHtml,
    showInBrowser,
    showInBrowserWith,

    -- * Parts
    plotlyCdnUrl,
    plotlyJsBundle,
    figureDivId,
    newPlotScript,
    scriptJson,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Data.Aeson (ToJSON, Value (..), encode)
import Data.Aeson.KeyMap qualified as KM
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Word (Word64)
import Graphics.Plotly.Internal.Bundle (bundle)
import Graphics.Plotly.Internal.Core (Figure (..), Obj (..))
import Graphics.Plotly.Version (plotlyJsVersion)
import Numeric (showHex)
import System.Directory (getTemporaryDirectory)
import System.IO (hClose)
import System.IO qualified as IO
import System.Info (os)
import System.Process (createProcess, getProcessExitCode, proc)

-- | How a page gets plotly.js.
data IncludePlotlyJs
  = -- | A @script@ tag loading this version from plotly's CDN.
    CDN Text
  | -- | The bundle vendored with the library, embedded in the page (about
    -- 4.7 MB). The page works offline.
    Inline
  | -- | Load the vendored version from the CDN when the figure is drawn,
    -- unless the page already has plotly.js, for pages that get HTML after
    -- they have loaded, like notebooks. (Not RequireJS: plotly.js 3 and
    -- later always define a global @Plotly@, and are no AMD module.)
    Dynamic
  | -- | Nothing: the page already has plotly.js.
    None
  deriving stock (Eq, Show)

-- | Options for 'toHtml'.
data HtmlOptions = HtmlOptions
  { -- | How the page gets plotly.js.
    plotlyJs :: IncludePlotlyJs,
    -- | A whole page, or a fragment to put in one.
    fullHtml :: Bool,
    -- | The id of the @div@ holding the plot. 'Nothing' makes one from the
    -- figure ('figureDivId'), so the same figure twice on one page needs ids
    -- of its own.
    divId :: Maybe Text,
    -- | Fill the width and height of the containing element, and follow it
    -- when it resizes. Sets the figure's @config.responsive@ unless the
    -- figure sets it.
    responsive :: Bool
  }
  deriving stock (Eq, Show)

-- | A whole page, responsive, loading the vendored plotly.js version from
-- the CDN.
defaultHtmlOptions :: HtmlOptions
defaultHtmlOptions =
  HtmlOptions
    { plotlyJs = CDN plotlyJsVersion,
      fullHtml = True,
      divId = Nothing,
      responsive = True
    }

-- | The figure as HTML. Any string in the figure is safe to put in it: none
-- can close the @script@ element.
toHtml :: HtmlOptions -> Figure g -> Text
toHtml opts fig0
  | fullHtml opts =
      T.unlines $
        [ "<!DOCTYPE html>",
          "<html>",
          "<head>",
          "<meta charset=\"utf-8\">"
        ]
          <> ["<style>html, body { height: 100%; margin: 0; }</style>" | responsive opts]
          <> scriptTags
          <> ["</head>", "<body>", plotDiv, plotScript, "</body>", "</html>"]
  | otherwise = T.unlines (["<div>"] <> scriptTags <> [plotDiv, plotScript, "</div>"])
  where
    fig
      | responsive opts = fig0 {config = setDefault "responsive" (Bool True) (config fig0)}
      | otherwise = fig0
    json = encode fig
    divId' = fromMaybe (hashDivId json) (divId opts)
    plotDiv =
      "<div id=\""
        <> escapeAttr divId'
        <> "\" class=\"plotly-graph-div\""
        <> (if responsive opts then " style=\"height: 100%; width: 100%;\"" else "")
        <> "></div>"
    plotScript = "<script>" <> drawScript (plotlyJs opts) divId' (escapeScript json) <> "</script>"
    scriptTags = case plotlyJs opts of
      CDN v -> ["<script src=\"" <> escapeAttr (plotlyCdnUrl v) <> "\" charset=\"utf-8\"></script>"]
      Inline -> ["<script>" <> plotlyJsBundle <> "</script>"]
      Dynamic -> []
      None -> []
    setDefault k v (Obj o) = Obj (KM.insertWith (\_ old -> old) k v o)

-- | Write the figure's HTML to a file, as UTF-8.
writeHtml :: HtmlOptions -> FilePath -> Figure g -> IO ()
writeHtml opts path fig = BS.writeFile path (encodeUtf8 (toHtml opts fig))

-- | Open the figure in a web browser: write a page, with plotly.js inline so
-- it works offline, to a temporary file and open it with @xdg-open@ (or
-- @open@ on macOS, @start@ on Windows). The file is left for the browser.
showInBrowser :: Figure g -> IO ()
showInBrowser = showInBrowserWith defaultHtmlOptions {plotlyJs = Inline}

-- | 'showInBrowser' with these options; the page is always a whole page.
showInBrowserWith :: HtmlOptions -> Figure g -> IO ()
showInBrowserWith opts fig = do
  tmp <- getTemporaryDirectory
  (path, h) <- IO.openBinaryTempFile tmp "plotlyhs.html"
  BS.hPut h (encodeUtf8 (toHtml opts {fullHtml = True} fig))
  hClose h
  (_, _, _, p) <- createProcess (opener path)
  -- Reap the opener without making the caller wait for it. It polls, as
  -- waitForProcess would block every thread of a program built without
  -- -threaded until the opener exits, which may be when the browser does.
  let reap = getProcessExitCode p >>= maybe (threadDelay 250000 >> reap) (const (pure ()))
  void (forkIO reap)
  where
    opener path = case os of
      "darwin" -> proc "open" [path]
      "mingw32" -> proc "cmd" ["/c", "start", "", path]
      _ -> proc "xdg-open" [path]

-- | The CDN's URL for this version of plotly.js's minified bundle.
plotlyCdnUrl :: Text -> Text
plotlyCdnUrl v = "https://cdn.plot.ly/plotly-" <> v <> ".min.js"

-- | The minified plotly.js bundle vendored with the library, at
-- 'plotlyJsVersion'.
plotlyJsBundle :: Text
plotlyJsBundle = decodeUtf8 bundle
{-# NOINLINE plotlyJsBundle #-}

-- | An id for the figure's @div@, from a hash of its JSON: the same for the
-- same figure.
figureDivId :: Figure g -> Text
figureDivId = hashDivId . encode

hashDivId :: BL.ByteString -> Text
hashDivId json = "plotly-" <> T.justifyRight 16 '0' (T.pack (showHex (fnv1a json) ""))
  where
    fnv1a :: BL.ByteString -> Word64
    fnv1a = BL.foldl' (\h b -> (h `xor` fromIntegral b) * 1099511628211) 14695981039346656037

-- | JavaScript that draws the figure, with @Plotly.newPlot@, in the @div@
-- with this id. Safe to put in a @script@ element: no string in the figure
-- can close it.
newPlotScript :: Text -> Figure g -> Text
newPlotScript divId' = drawScript None divId' . scriptJson

-- | The script drawing a figure, given as 'scriptJson'.
drawScript :: IncludePlotlyJs -> Text -> Text -> Text
drawScript Dynamic divId' figJson =
  T.concat
    [ "(function () {",
      " var url = ",
      scriptJson (plotlyCdnUrl plotlyJsVersion),
      ";",
      " var loaded = window.Plotly ? Promise.resolve(window.Plotly)",
      " : window.plotlyhsLoading || (window.plotlyhsLoading = new Promise(function (resolve, reject) {",
      " var s = document.createElement(\"script\");",
      " s.src = url;",
      " s.onload = function () { resolve(window.Plotly); };",
      " s.onerror = function () { window.plotlyhsLoading = null; reject(new Error(\"could not load \" + url)); };",
      " document.head.appendChild(s); }));",
      " loaded.then(function (Plotly) { return Plotly.newPlot(",
      scriptJson divId',
      ", ",
      figJson,
      "); }).catch(function (e) { var div = document.getElementById(",
      scriptJson divId',
      "); if (div) { div.textContent = (e && e.message) || String(e); } });",
      " })();"
    ]
drawScript _ divId' figJson = "Plotly.newPlot(" <> scriptJson divId' <> ", " <> figJson <> ");"

-- | JSON safe to put in a @script@ element. @<@, @>@ and @&@ are escaped, so
-- nothing in it can close the element, and U+2028 and U+2029, which end a
-- line in older JavaScript. All of them only occur inside strings, where
-- @\\u@ escapes mean the same.
scriptJson :: (ToJSON a) => a -> Text
scriptJson = escapeScript . encode

escapeScript :: BL.ByteString -> Text
escapeScript json = foldr (uncurry T.replace) (decodeUtf8 (BL.toStrict json)) escapes
  where
    -- No replacement holds a character another replaces.
    escapes =
      [ ("<", "\\u003c"),
        (">", "\\u003e"),
        ("&", "\\u0026"),
        ("\x2028", "\\u2028"),
        ("\x2029", "\\u2029")
      ]

escapeAttr :: Text -> Text
escapeAttr = T.concatMap $ \case
  '&' -> "&amp;"
  '<' -> "&lt;"
  '>' -> "&gt;"
  '"' -> "&quot;"
  '\'' -> "&#39;"
  c -> T.singleton c
