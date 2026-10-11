{-# LANGUAGE OverloadedStrings #-}

-- |
-- Render figures to HTML with blaze-html.
--
-- @
-- page :: Html
-- page = H.docTypeHtml $ do
--   H.head plotlyCDN
--   H.body (plotly "myDiv" fig)
-- @
module Graphics.Plotly.Blaze
  ( plotlyCDN,
    plotly,
    plotlyDiv,
    plotlyJS,
  )
where

import Data.Text (Text)
import Graphics.Plotly.Core (Figure)
import Graphics.Plotly.Internal.Script (newPlotScript)
import Graphics.Plotly.Version (plotlyJsVersion)
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

-- | A @script@ tag that loads plotly.js from its CDN, at the version the
-- library is generated from.
plotlyCDN :: H.Html
plotlyCDN = H.script H.! A.src (H.toValue ("https://cdn.plot.ly/plotly-" <> plotlyJsVersion <> ".min.js")) $ ""

-- | A @div@ with this id, and the script that draws the figure in it.
plotly :: Text -> Figure -> H.Html
plotly divId fig = plotlyDiv divId >> plotlyJS divId fig

-- | A @div@ with this id.
plotlyDiv :: Text -> H.Html
plotlyDiv divId = H.div H.! A.id (H.toValue divId) $ ""

-- | The script that draws the figure in the @div@ with this id.
plotlyJS :: Text -> Figure -> H.Html
plotlyJS divId fig = H.script (H.preEscapedText (newPlotScript divId fig))
