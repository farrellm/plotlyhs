{-# LANGUAGE OverloadedStrings #-}

-- |
-- Render figures to HTML with lucid. Depend on @plotlyhs:lucid@.
--
-- @
-- page :: Html ()
-- page = doctypehtml_ $ do
--   head_ plotlyCDN
--   body_ (plotly "myDiv" fig)
-- @
module Graphics.Plotly.Lucid
  ( plotlyCDN,
    plotly,
    plotlyWith,
    plotlyDiv,
    plotlyJS,
  )
where

import Data.Text (Text)
import Graphics.Plotly.Core (Figure)
import Graphics.Plotly.Html (HtmlOptions (..), newPlotScript, plotlyCdnUrl)
import Graphics.Plotly.Html qualified as Html
import Graphics.Plotly.Version (plotlyJsVersion)
import Lucid

-- | A @script@ tag that loads plotly.js from its CDN, at the version the
-- library is generated from.
plotlyCDN :: (Monad m) => HtmlT m ()
plotlyCDN = script_ [src_ (plotlyCdnUrl plotlyJsVersion)] ("" :: Text)

-- | A @div@ with this id, and the script that draws the figure in it.
plotly :: (Monad m) => Text -> Figure g -> HtmlT m ()
plotly divId' fig = plotlyDiv divId' >> plotlyJS divId' fig

-- | The figure as 'Html.toHtml' renders it with these options, always as a
-- fragment.
plotlyWith :: (Monad m) => HtmlOptions -> Figure g -> HtmlT m ()
plotlyWith opts fig = toHtmlRaw (Html.toHtml opts {fullHtml = False} fig)

-- | A @div@ with this id.
plotlyDiv :: (Monad m) => Text -> HtmlT m ()
plotlyDiv divId' = div_ [id_ divId'] ""

-- | The script that draws the figure in the @div@ with this id.
plotlyJS :: (Monad m) => Text -> Figure g -> HtmlT m ()
plotlyJS divId' fig = script_ (newPlotScript divId' fig)
