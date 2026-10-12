# Plotlyhs 

A Haskell library for data visualization, using [Plotly.js](https://plotly.com/javascript/). Generate interactive charts in HTML from your Haskell data.

This is a fork of plotlyhs, the library begun by @glutamate.

## Example

For more, [see the examples page](https://farrellm.github.io/plotlyhs/)

```haskell
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

import Graphics.Plotly
import Graphics.Plotly.Html (defaultHtmlOptions, showInBrowser, writeHtml)
import Graphics.Plotly.Image (defaultImageOptions, writeImage)
import Optics.Core

main :: IO ()
main = do
  writeHtml defaultHtmlOptions "plot.html" fig
  writeImage defaultImageOptions "plot.png" fig
  showInBrowser fig

fig :: Figure
fig =
  figure [someTrace myTrace]
    & #layout % #title % #text ?~ "My plot"

myTrace :: Trace Scatter
myTrace =
  scatter
    & #x ?~ dataArray [1, 2, 3, 4 :: Double]
    & #y ?~ dataArray [500, 3000, 700, 200 :: Double]
    & #mode ?~ Flags (ModeLines :| [ModeMarkers])
```

## Output

- **HTML** (`Graphics.Plotly.Html`): `toHtml` and `writeHtml` render a whole
  page or a fragment. plotly.js comes from its CDN, pinned to the vendored
  version (`CDN`), is embedded (`Inline`), is loaded when the figure is drawn
  (`Dynamic`, for notebooks), or is left to the page (`None`). `showInBrowser`
  opens a figure in a web browser.
- **JSON** (`Graphics.Plotly.Json`): `toJson`, `fromJson`, `writeJson` and
  `readJson`, in plotly.js's figure format, which plotly.py
  (`plotly.io.from_json`) and PlotlyBase.jl read too.
- **Images** (`Graphics.Plotly.Image`): `writeImage` writes PNG, JPEG, WebP,
  SVG or PDF. It runs a headless Chrome or Chromium (found on the `PATH`, or
  set `PLOTLYHS_CHROME`), with no Python needed; without one, it uses Python's
  `kaleido` (version 1 or later) if it is installed.
- **lucid and blaze-html**: the `plotlyhs:lucid` and `plotlyhs:blaze`
  sub-libraries provide `Graphics.Plotly.Lucid` and `Graphics.Plotly.Blaze`,
  so the core depends on neither.
- **Jupyter**: with the `plotlyhs-jupyter` package, IHaskell notebooks display
  figures and traces.

Every trace type and attribute of plotly.js is available, generated from
plotly.js's schema (`vendor/plotly.js/plot-schema.json`) by `plotlyhs-gen` into
`src-gen/`. Regenerate it with `cabal run plotlyhs-gen` after updating the
vendored plotly.js.
