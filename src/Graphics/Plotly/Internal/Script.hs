{-# LANGUAGE OverloadedStrings #-}

-- | The JavaScript that draws a figure, shared by the HTML renderers.
module Graphics.Plotly.Internal.Script (newPlotScript) where

import Data.Aeson (ToJSON, encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Graphics.Plotly.Core (Figure)

-- | @Plotly.newPlot@ drawing the figure in the @div@ with this id. Safe to
-- put in a @script@ element: no string in the figure can close it.
newPlotScript :: Text -> Figure -> Text
newPlotScript divId fig = "Plotly.newPlot(" <> json divId <> ", " <> json fig <> ");"

-- | JSON with @<@, @>@ and @&@ escaped. They only occur inside strings,
-- where @\\u@ escapes mean the same.
json :: (ToJSON a) => a -> Text
json = T.concatMap escape . decodeUtf8 . toStrict . encode
  where
    escape = \case
      '<' -> "\\u003c"
      '>' -> "\\u003e"
      '&' -> "\\u0026"
      c -> T.singleton c
