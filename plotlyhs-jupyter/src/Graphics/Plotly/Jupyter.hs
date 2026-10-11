{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Jupyter display support for plotlyhs, through IHaskell: importing this
-- module lets a notebook cell show a t'Figure', or a single 'Trace'.
--
-- A figure is shown as HTML that loads plotly.js from its CDN, once per
-- page, and draws the figure. The notebook must be trusted for its scripts
-- to run, as for plotly.py's HTML output.
--
-- IHaskell 0.13 sends a custom MIME type's data as a JSON string, so the
-- @application/vnd.plotly.v1+json@ output, whose renderers expect a JSON
-- object, is not sent; 'plotlyMimeType' is for when it can be.
module Graphics.Plotly.Jupyter
  ( notebookHtml,
    plotlyMimeType,
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import Graphics.Plotly.Core (Figure (..), Trace, figure, someTrace)
import Graphics.Plotly.Html (HtmlOptions (..), IncludePlotlyJs (..), defaultHtmlOptions, toHtml)
import IHaskell.Display (Display (..), IHaskellDisplay (..), html, plain)
import System.IO.Unsafe (unsafePerformIO)

instance IHaskellDisplay Figure where
  display fig = do
    n <- atomicModifyIORef' counter (\i -> (i + 1, i))
    -- The kernel's start time and a count, so ids differ across kernel
    -- sessions, whose outputs share a notebook page.
    let divId' = "plotlyhs-" <> T.pack (show start) <> "-" <> T.pack (show n)
    pure (Display [plain (summary fig), html (T.unpack (notebookHtml divId' fig))])

instance IHaskellDisplay (Trace t) where
  display tr = display (figure [someTrace tr])

-- | The HTML a notebook shows for the figure, in a @div@ with this id: a
-- fragment that loads plotly.js from the CDN unless the page has it, and
-- fills the cell's width.
notebookHtml :: Text -> Figure -> Text
notebookHtml divId' =
  toHtml
    defaultHtmlOptions
      { plotlyJs = Dynamic,
        fullHtml = False,
        divId = Just divId',
        responsive = False
      }

-- | The MIME type of plotly.js's figure JSON, which JupyterLab and VS Code
-- render with plotly.js.
plotlyMimeType :: Text
plotlyMimeType = "application/vnd.plotly.v1+json"

summary :: Figure -> String
summary fig = case length (data_ fig) of
  1 -> "<Figure: 1 trace>"
  n -> "<Figure: " <> show n <> " traces>"

counter :: IORef Int
counter = unsafePerformIO (newIORef 0)
{-# NOINLINE counter #-}

start :: Integer
start = unsafePerformIO (floor . (* 1000) <$> getPOSIXTime)
{-# NOINLINE start #-}
