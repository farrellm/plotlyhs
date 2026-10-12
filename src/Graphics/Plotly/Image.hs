{-# LANGUAGE CPP #-}
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

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (Exception, IOException, throwIO, try)
import Control.Monad (filterM, unless)
import Data.Aeson (Value (..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.Either (fromRight)
import Data.Foldable (asum)
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Scientific (toRealFloat)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, decodeUtf8Lenient, encodeUtf8)
import Graphics.Plotly.Core (Figure (..), attr)
import Graphics.Plotly.Html (scriptJson)
import Graphics.Plotly.Internal.Bundle (bundle)
import Numeric (showHex)
import Optics.Core ((^.))
import System.Directory (doesFileExist, findExecutable)
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath (takeExtension, (</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Info (os)
#if !defined(mingw32_HOST_OS)
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (getFileSystemEncoding, getForeignEncoding)
import System.Posix.User (getEffectiveUserID)
#endif
import System.Process (CreateProcess (..), StdStream (..), proc, readProcessWithExitCode, waitForProcess, withCreateProcess)

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
    -- | A factor scaling a PNG, JPEG or WebP image's resolution, or an
    -- SVG's @width@ and @height@. A PDF keeps the given size.
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
      -- Rounded, as a size may be fractional.
      Just (Number n) | px <- toRealFloat n :: Double, px >= 1, px < 1e9 -> round px
      _ -> def

-- | Chrome if 'findChrome' finds one (with @--no-sandbox@ when running as
-- root, which Chrome refuses without), or else Python with @kaleido@.
findImageEngine :: IO (Maybe ImageEngine)
findImageEngine = do
  chrome <- findChrome
  case chrome of
    Just exe -> do
      root <- isRoot
      pure (Just (Chrome exe ["--no-sandbox" | root]))
    Nothing -> fmap Kaleido <$> findKaleido

-- | Whether the program runs as root.
isRoot :: IO Bool
#if defined(mingw32_HOST_OS)
isRoot = pure False
#else
isRoot = (== 0) <$> getEffectiveUserID
#endif

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
        Nothing -> listToMaybe <$> filterM doesFileExist installed
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
  -- Chrome's profile and PDF paths go to it as arguments, which lose what
  -- the locale can't encode: Chrome would then write them somewhere else.
  -- (Relative paths, with Chrome run in the directory, make it hang.)
  intact <- argIntact dir
  unless intact $
    throwIO (ImageError ("the temporary directory " <> dir <> " can't be passed to Chrome in this locale; set TMPDIR to an ASCII path"))
  BS.writeFile (dir </> "plotly.min.js") bundle
  let page = dir </> "image.html"
      drawn = if format spec == Pdf then Svg else format spec
  BS.writeFile page (encodeUtf8 (imagePage spec {format = drawn} fig))
  pageUrl <- fileUrl page
  dom <- runChrome dir ["--dump-dom", "--virtual-time-budget=60000", pageUrl]
  img <- case BS.breakSubstring "</pre>" . snd . BS.breakSubstring marker $ dom of
    (out, _)
      | Just b64 <- BS.stripPrefix (marker <> "OK:") out ->
          either (\e -> throwIO (ImageError ("Chrome returned bad base64: " <> e))) pure (Base64.decode b64)
      | Just err <- BS.stripPrefix (marker <> "ERROR:") out ->
          throwIO (ImageError ("plotly.js could not draw the image: " <> T.unpack (unescapeText (decodeUtf8Lenient err))))
      | otherwise -> throwIO (ImageError "Chrome stopped before plotly.js drew the image")
  if format spec /= Pdf
    then pure img
    else do
      let svgPage = dir </> "pdf.html"
          pdf = dir </> "image.pdf"
      BS.writeFile svgPage (encodeUtf8 (pdfPage spec img))
      svgUrl <- fileUrl svgPage
      _ <- runChrome dir ["--no-pdf-header-footer", "--print-to-pdf-no-header", "--print-to-pdf=" <> pdf, svgUrl]
      BS.readFile pdf
  where
    marker = "<pre id=\"plotlyhs-image\">"
    runChrome dir extra = do
      (code, out, err) <-
        readProcessBytes . proc exe $
          ["--headless", "--disable-gpu", "--no-first-run", "--no-default-browser-check", "--user-data-dir=" <> (dir </> "profile")]
            <> args
            <> extra
      unless (code == ExitSuccess) $
        throwIO (ImageError ("Chrome failed (" <> show code <> "): " <> T.unpack (decodeUtf8Lenient err)))
      pure out

-- | Run a program, and read its output and errors as bytes: the DOM Chrome
-- prints holds the figure's text, which the locale's encoding (ASCII in the
-- C locale) may not decode.
readProcessBytes :: CreateProcess -> IO (ExitCode, ByteString, ByteString)
readProcessBytes cp =
  -- An empty standard input, not a closed one, which a child may reuse.
  withCreateProcess cp {std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe} $ \min' mout merr p ->
    case (min', mout, merr) of
      (Just hin, Just hout, Just herr) -> do
        hClose hin
        -- Read the errors alongside the output, so neither pipe fills.
        errVar <- newEmptyMVar
        _ <- forkIO (try @IOException (BS.hGetContents herr) >>= putMVar errVar)
        out <- BS.hGetContents hout
        err <- fromRight "" <$> takeMVar errVar
        code <- waitForProcess p
        pure (code, out, err)
      _ -> throwIO (ImageError "could not read a program's output")

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
    optsJson = scriptJson (specJson spec)

-- | The image options plotly.js's and kaleido's @toImage@ take.
specJson :: Spec -> Value
specJson spec =
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

-- | A @file:@ URL for an absolute path, percent-encoded, so a @#@, @%@ or
-- @?@ in it stays part of the path.
fileUrl :: FilePath -> IO String
fileUrl path = ("file://" <>) . concatMap escape . BS.unpack <$> pathBytes slashed
  where
    slashed = case path of
      '/' : _ -> path
      _ -> '/' : map (\c -> if c == '\\' then '/' else c) path
    escape b
      | isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("-._~/:" :: String) = [c]
      | otherwise = '%' : (if b < 16 then ('0' :) else id) (showHex b "")
      where
        c = chr (fromIntegral b)

-- | A path's bytes: on POSIX, in the file system's encoding, which keeps a
-- non-ASCII path's bytes in the C locale (where the path holds escapes).
pathBytes :: FilePath -> IO ByteString
#if defined(mingw32_HOST_OS)
pathBytes = pure . encodeUtf8 . T.pack
#else
pathBytes path = do
  enc <- getFileSystemEncoding
  GHC.withCStringLen enc path BS.packCStringLen
#endif

-- | Whether an argument reaches a program unchanged. On POSIX, @process@
-- encodes arguments in the locale's encoding, dropping what it can't encode
-- (a non-ASCII path in the C locale), not in the file system's.
argIntact :: String -> IO Bool
#if defined(mingw32_HOST_OS)
argIntact _ = pure True
#else
argIntact arg = do
  enc <- getForeignEncoding
  (==) <$> GHC.withCStringLen enc arg BS.packCStringLen <*> pathBytes arg
#endif

-- | The text of an element as the DOM Chrome prints escapes it.
unescapeText :: Text -> Text
unescapeText =
  T.replace "&amp;" "&" . T.replace "&lt;" "<" . T.replace "&gt;" ">" . T.replace "&nbsp;" "\x00a0"

-- | Have kaleido draw the image, with the vendored plotly.js.
kaleidoImage :: FilePath -> Spec -> Figure -> IO ByteString
kaleidoImage python spec fig = withSystemTempDirectory "plotlyhs" $ \dir -> do
  BS.writeFile (dir </> "plotly.min.js") bundle
  Aeson.encodeFile (dir </> "input.json") $
    object
      [ "figure" .= fig,
        "opts" .= specJson spec
      ]
  -- Python runs in the directory and is given no paths: in the C locale,
  -- arguments lose a non-ASCII path's characters, and JSON would too.
  (code, _, err) <- readProcessBytes (proc python ["-c", kaleidoScript]) {cwd = Just dir}
  unless (code == ExitSuccess) $
    throwIO (ImageError ("kaleido failed (" <> show code <> "): " <> T.unpack (decodeUtf8Lenient err)))
  BS.readFile (dir </> "image")

-- | Draws @input.json@'s figure, with @plotly.min.js@, to @image@, all in
-- the working directory.
kaleidoScript :: String
kaleidoScript =
  unlines
    [ "import json, os, kaleido",
      "with open('input.json', encoding='utf-8') as f:",
      "    spec = json.load(f)",
      "image = kaleido.calc_fig_sync(spec['figure'], opts=spec['opts'], kopts={'plotlyjs': os.path.abspath('plotly.min.js')})",
      "with open('image', 'wb') as f:",
      "    f.write(image)"
    ]
