{-# LANGUAGE OverloadedStrings #-}

-- |
-- Render figures to HTML with blaze-html. Depend on @plotlyhs:blaze@.
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
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A

-- | A @script@ tag that loads plotly.js from its CDN, at the version the
-- library is generated from.
plotlyCDN :: H.Html
plotlyCDN = H.script H.! A.src (H.toValue (plotlyCdnUrl plotlyJsVersion)) $ ""

-- | A @div@ with this id, and the script that draws the figure in it.
plotly :: Text -> Figure g -> H.Html
plotly divId' fig = plotlyDiv divId' >> plotlyJS divId' fig

-- | The figure as 'Html.toHtml' renders it with these options, always as a
-- fragment.
plotlyWith :: HtmlOptions -> Figure g -> H.Html
plotlyWith opts fig = H.preEscapedText (Html.toHtml opts {fullHtml = False} fig)

-- | A @div@ with this id.
plotlyDiv :: Text -> H.Html
plotlyDiv divId' = H.div H.! A.id (H.toValue divId') $ ""

-- | The script that draws the figure in the @div@ with this id.
plotlyJS :: Text -> Figure g -> H.Html
plotlyJS divId' fig = H.script (H.preEscapedText (newPlotScript divId' fig))
