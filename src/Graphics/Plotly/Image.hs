{-# LANGUAGE OverloadedStrings #-}

-- |
-- Static images of figures: PNG, JPEG, WebP, SVG and PDF.
--
-- @
-- writeImage defaultImageOptions "plot.png" fig
-- @
--
-- Images are drawn by plotly.js itself, in a headless Chrome or Chromium,
-- with the plotly.js bundle vendored with the library. Without a Chrome,
-- Python's @kaleido@ (version 1 or later), which drives a Chrome of its own,
-- is used if it is installed.
module Graphics.Plotly.Image
  ( -- * Writing images
    writeImage,
    toImage,

    -- * Options
    ImageOptions (..),
    defaultImageOptions,
    ImageFormat (..),
    imageFormatFromPath,
    ImageEngine (..),
    findImageEngine,
    findChrome,

    -- * Errors
    ImageError (..),
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad (filterM, unless)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as BC
import Data.Char (toLower)
import Data.Foldable (asum)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Graphics.Plotly.Core (Figure (..), attr)
import Graphics.Plotly.Html (plotlyJsBundle, scriptJson)
import Optics.Core ((^.))
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
import System.Process (readProcessWithExitCode)

-- | An image format.
data ImageFormat = Png | Jpeg | Webp | Svg | Pdf
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The format a file name's extension names: @.png@, @.jpg@ or @.jpeg@,
-- @.webp@, @.svg@ or @.pdf@, in any case.
imageFormatFromPath :: FilePath -> Maybe ImageFormat
imageFormatFromPath path = case map toLower (takeExtension path) of
  ".png" -> Just Png
  ".jpg" -> Just Jpeg
  ".jpeg" -> Just Jpeg
  ".webp" -> Just Webp
  ".svg" -> Just Svg
  ".pdf" -> Just Pdf
  _ -> Nothing

-- | What draws the image.
data ImageEngine
  = -- | A Chrome or Chromium executable, run headless, and extra arguments
    -- for it (such as @--no-sandbox@, which Chrome needs to run as root).
    Chrome FilePath [String]
  | -- | A Python executable with @kaleido@ 1 or later installed.
    Kaleido FilePath
  deriving stock (Eq, Show)

-- | Options for 'writeImage' and 'toImage'.
data ImageOptions = ImageOptions
  { -- | The format. 'Nothing': from the file name's extension for
    -- 'writeImage', PNG for 'toImage'.
    imageFormat :: Maybe ImageFormat,
    -- | The width in pixels. 'Nothing': the layout's @width@, or 700.
    imageWidth :: Maybe Int,
    -- | The height in pixels. 'Nothing': the layout's @height@, or 500.
    imageHeight :: Maybe Int,
    -- | A factor scaling a PNG, JPEG or WebP image's resolution.
    imageScale :: Double,
    -- | What draws the image. 'Nothing': 'findImageEngine'.
    imageEngine :: Maybe ImageEngine
  }
  deriving stock (Eq, Show)

-- | The format from the file name (or PNG), the layout's size (or 700 by
-- 500), scale 1, and whatever 'findImageEngine' finds.
defaultImageOptions :: ImageOptions
defaultImageOptions = ImageOptions Nothing Nothing Nothing 1 Nothing

-- | Why an image could not be made.
newtype ImageError = ImageError String
  deriving stock (Show)

instance Exception ImageError

-- | Write an image of the figure to a file. Throws an t'ImageError' if it
-- cannot be made.
writeImage :: ImageOptions -> FilePath -> Figure -> IO ()
writeImage opts path fig = do
  fmt <- case imageFormat opts of
    Just f -> pure f
    Nothing -> maybe (throwIO (ImageError ("no image format for the file name " <> show path))) pure (imageFormatFromPath path)
  BS.writeFile path =<< toImage opts {imageFormat = Just fmt} fig

-- | An image of the figure, in its format's bytes. Throws an t'ImageError'
-- if it cannot be made.
toImage :: ImageOptions -> Figure -> IO ByteString
toImage opts fig = do
  engine <- maybe findImageEngine (pure . Just) (imageEngine opts)
  case engine of
    Just (Chrome exe args) -> chromeImage exe args spec fig
    Just (Kaleido python) -> kaleidoImage python spec fig
    Nothing ->
      throwIO . ImageError $
        "no Chrome or Chromium found (set PLOTLYHS_CHROME to one), "
          <> "and no Python with kaleido installed"
  where
    spec =
      Spec
        { format = fromMaybe Png (imageFormat opts),
          width = fromMaybe (layoutSize "width" 700) (imageWidth opts),
          height = fromMaybe (layoutSize "height" 500) (imageHeight opts),
          scale = imageScale opts
        }
    layoutSize k def = case layout fig ^. attr k of
      Just (Number n) | Just px <- toBoundedInteger n, px > 0 -> px
      _ -> def

-- | Chrome if 'findChrome' finds one, or else Python with @kaleido@.
findImageEngine :: IO (Maybe ImageEngine)
findImageEngine = do
  chrome <- findChrome
  case chrome of
    Just exe -> pure (Just (Chrome exe []))
    Nothing -> fmap Kaleido <$> findKaleido

-- | A Chrome or Chromium executable: @$PLOTLYHS_CHROME@ if it is set, or
-- else the first of @google-chrome-stable@, @google-chrome@, @chromium@,
-- @chromium-browser@ and @chrome@ on the @PATH@, or else one installed in
-- the usual place on macOS or Windows.
findChrome :: IO (Maybe FilePath)
findChrome = do
  env <- lookupEnv "PLOTLYHS_CHROME"
  case env of
    Just exe | not (null exe) -> pure (Just exe)
    _ -> do
      onPath <- asum <$> traverse findExecutable names
      case onPath of
        Just exe -> pure (Just exe)
        Nothing -> headMaybe <$> filterM doesFileExist installed
  where
    names = ["google-chrome-stable", "google-chrome", "chromium", "chromium-browser", "chrome"]
    installed = case os of
      "darwin" ->
        [ "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome",
          "/Applications/Chromium.app/Contents/MacOS/Chromium"
        ]
      "mingw32" ->
        [ "C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe",
          "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe"
        ]
      _ -> []
    headMaybe = \case
      x : _ -> Just x
      [] -> Nothing

-- | @python3@ (or @python@) on the @PATH@, if it can import @kaleido@.
findKaleido :: IO (Maybe FilePath)
findKaleido = do
  pythons <- traverse findExecutable ["python3", "python"]
  firstM hasKaleido (catMaybes pythons)
  where
    hasKaleido python = do
      (code, _, _) <- readProcessWithExitCode python ["-c", "import kaleido"] ""
      pure (code == ExitSuccess)
    firstM _ [] = pure Nothing
    firstM p (x : xs) = do
      ok <- p x
      if ok then pure (Just x) else firstM p xs

data Spec = Spec
  { format :: ImageFormat,
    width :: Int,
    height :: Int,
    scale :: Double
  }

formatName :: ImageFormat -> Text
formatName = \case
  Png -> "png"
  Jpeg -> "jpeg"
  Webp -> "webp"
  Svg -> "svg"
  Pdf -> "pdf"

-- | Have plotly.js draw the image in headless Chrome. The page puts the
-- image, base64-encoded, in an element, and Chrome prints the page's DOM
-- once nothing is left to do. A PDF is drawn as SVG and printed.
chromeImage :: FilePath -> [String] -> Spec -> Figure -> IO ByteString
chromeImage exe args spec fig = withSystemTempDirectory "plotlyhs" $ \dir -> do
  BS.writeFile (dir </> "plotly.min.js") (encodeUtf8 plotlyJsBundle)
  let page = dir </> "image.html"
      drawn = if format spec == Pdf then Svg else format spec
  BS.writeFile page (encodeUtf8 (imagePage spec {format = drawn} fig))
  dom <- runChrome dir ["--dump-dom", "--virtual-time-budget=60000", fileUrl page]
  img <- case BS.breakSubstring "</pre>" . snd . BS.breakSubstring marker . BC.pack $ dom of
    (out, _)
      | Just b64 <- BS.stripPrefix (marker <> "OK:") out ->
          either (\e -> throwIO (ImageError ("Chrome returned bad base64: " <> e))) pure (Base64.decode b64)
      | Just err <- BS.stripPrefix (marker <> "ERROR:") out ->
          throwIO (ImageError ("plotly.js could not draw the image: " <> T.unpack (decodeUtf8 err)))
      | otherwise -> throwIO (ImageError "Chrome stopped before plotly.js drew the image")
  if format spec /= Pdf
    then pure img
    else do
      let svgPage = dir </> "pdf.html"
          pdf = dir </> "image.pdf"
      BS.writeFile svgPage (encodeUtf8 (pdfPage spec img))
      _ <- runChrome dir ["--no-pdf-header-footer", "--print-to-pdf-no-header", "--print-to-pdf=" <> pdf, fileUrl svgPage]
      BS.readFile pdf
  where
    marker = "<pre id=\"plotlyhs-image\">"
    runChrome dir extra = do
      (code, out, err) <-
        readProcessWithExitCode
          exe
          ( ["--headless", "--disable-gpu", "--no-first-run", "--no-default-browser-check", "--user-data-dir=" <> (dir </> "profile")]
              <> args
              <> extra
          )
          ""
      unless (code == ExitSuccess) $
        throwIO (ImageError ("Chrome failed (" <> show code <> "): " <> err))
      pure out

-- | A page that draws the figure's image and writes it to the
-- @plotlyhs-image@ element: @OK:@ and base64, or @ERROR:@ and why.
imagePage :: Spec -> Figure -> Text
imagePage spec fig =
  T.unlines
    [ "<!DOCTYPE html>",
      "<html><head><meta charset=\"utf-8\"><script src=\"plotly.min.js\"></script></head>",
      "<body><pre id=\"plotlyhs-image\"></pre><script>",
      "(function () {",
      "  var out = document.getElementById(\"plotlyhs-image\");",
      "  var done = function (s) { out.textContent = s; };",
      "  var fail = function (e) { done(\"ERROR:\" + e); };",
      "  var toBase64 = function (url) {",
      "    var comma = url.indexOf(\",\");",
      "    if (/;base64$/.test(url.slice(0, comma))) return url.slice(comma + 1);",
      "    var bytes = new TextEncoder().encode(decodeURIComponent(url.slice(comma + 1)));",
      "    var s = \"\";",
      "    for (var i = 0; i < bytes.length; i += 8192)",
      "      s += String.fromCharCode.apply(null, bytes.subarray(i, i + 8192));",
      "    return btoa(s);",
      "  };",
      "  try {",
      "    Plotly.toImage(" <> figJson <> ", " <> optsJson <> ")",
      "      .then(function (url) { done(\"OK:\" + toBase64(url)); }, fail);",
      "  } catch (e) { fail(e); }",
      "})();",
      "</script></body></html>"
    ]
  where
    figJson = scriptJson fig
    optsJson =
      scriptJson $
        object
          [ "format" .= formatName (format spec),
            "width" .= width spec,
            "height" .= height spec,
            "scale" .= scale spec
          ]

-- | A page of the image's size showing this SVG, to print as a PDF.
pdfPage :: Spec -> ByteString -> Text
pdfPage spec svg =
  T.unlines
    [ "<!DOCTYPE html>",
      "<html><head><meta charset=\"utf-8\"><style>",
      "@page { size: " <> w <> "px " <> h <> "px; margin: 0; }",
      "html, body { margin: 0; padding: 0; }",
      "img { display: block; width: " <> w <> "px; height: " <> h <> "px; }",
      "</style></head>",
      "<body><img src=\"data:image/svg+xml;base64," <> decodeUtf8 (Base64.encode svg) <> "\"></body></html>"
    ]
  where
    w = T.pack (show (width spec))
    h = T.pack (show (height spec))

fileUrl :: FilePath -> String
fileUrl path = case path of
  '/' : _ -> "file://" <> path
  _ -> "file:///" <> map (\c -> if c == '\\' then '/' else c) path

-- | Have kaleido draw the image, with the vendored plotly.js.
kaleidoImage :: FilePath -> Spec -> Figure -> IO ByteString
kaleidoImage python spec fig = withSystemTempDirectory "plotlyhs" $ \dir -> do
  let bundlePath = dir </> "plotly.min.js"
      input = dir </> "input.json"
      output = dir </> "image"
  BS.writeFile bundlePath (encodeUtf8 plotlyJsBundle)
  Aeson.encodeFile input $
    object
      [ "figure" .= fig,
        "plotlyjs" .= bundlePath,
        "output" .= output,
        "opts"
          .= object
            [ "format" .= formatName (format spec),
              "width" .= width spec,
              "height" .= height spec,
              "scale" .= scale spec
            ]
      ]
  (code, _, err) <- readProcessWithExitCode python ["-c", kaleidoScript, input] ""
  unless (code == ExitSuccess) $
    throwIO (ImageError ("kaleido failed (" <> show code <> "): " <> err))
  BS.readFile output

kaleidoScript :: String
kaleidoScript =
  unlines
    [ "import json, sys, kaleido",
      "with open(sys.argv[1]) as f:",
      "    spec = json.load(f)",
      "image = kaleido.calc_fig_sync(spec['figure'], opts=spec['opts'], kopts={'plotlyjs': spec['plotlyjs']})",
      "with open(spec['output'], 'wb') as f:",
      "    f.write(image)"
    ]
