{-# LANGUAGE OverloadedStrings #-}

-- |
-- Render figures to HTML with lucid.
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
    plotlyDiv,
    plotlyJS,
  )
where

import Data.Aeson (encode, toJSON)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Graphics.Plotly.Core (Figure)
import Graphics.Plotly.Version (plotlyJsVersion)
import Lucid

-- | A @script@ tag that loads plotly.js from its CDN, at the version the
-- library is generated from.
plotlyCDN :: (Monad m) => HtmlT m ()
plotlyCDN = script_ [src_ ("https://cdn.plot.ly/plotly-" <> plotlyJsVersion <> ".min.js")] ("" :: Text)

-- | A @div@ with this id, and the script that draws the figure in it.
plotly :: (Monad m) => Text -> Figure -> HtmlT m ()
plotly divId fig = plotlyDiv divId >> plotlyJS divId fig

-- | A @div@ with this id.
plotlyDiv :: (Monad m) => Text -> HtmlT m ()
plotlyDiv divId = div_ [id_ divId] ""

-- | The script that draws the figure in the @div@ with this id.
plotlyJS :: (Monad m) => Text -> Figure -> HtmlT m ()
plotlyJS divId fig =
  script_ ("Plotly.newPlot(" <> json (toJSON divId) <> ", " <> json (toJSON fig) <> ");")
  where
    json = decodeUtf8 . toStrict . encode
