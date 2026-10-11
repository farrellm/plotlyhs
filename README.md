# Plotlyhs 

A Haskell library for data visualization, using [Plotly.js](https://plotly.com/javascript/). Generate interactive charts in HTML from your Haskell data.

This is a fork of plotlyhs, the library begun by @glutamate.

## Example

For more, [see the examples page](https://farrellm.github.io/plotlyhs/)

```haskell
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text.Lazy.IO qualified as T
import Graphics.Plotly
import Graphics.Plotly.Lucid (plotly, plotlyCDN)
import Lucid
import Optics.Core

main :: IO ()
main =
  T.writeFile "test.html" $ renderText $ doctypehtml_ $ do
    head_ $ do
      meta_ [charset_ "utf-8"]
      plotlyCDN
    body_ $ plotly "myDiv" fig

fig :: Figure
fig =
  figure [someTrace myTrace]
    & #layout % #title % #text ?~ "My plot"

myTrace :: Trace Scatter
myTrace =
  scatter
    & #x ?~ dataArray [1, 2, 3, 4 :: Double]
    & #y ?~ dataArray [500, 3000, 700, 200 :: Double]
    & #mode ?~ Flags [ModeLines, ModeMarkers]
```

Every trace type and attribute of plotly.js is available, generated from
plotly.js's schema (`vendor/plotly.js/plot-schema.json`) by `plotlyhs-gen` into
`src-gen/`. Regenerate it with `cabal run plotlyhs-gen` after updating the
vendored plotly.js.
