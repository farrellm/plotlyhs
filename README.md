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

## Subplots, templates and colours

```haskell
import Graphics.Plotly.Colors.Qualitative qualified as Qualitative
import Graphics.Plotly.Template qualified as Template

panels :: Figure
panels =
  makeSubplots (subplotOptions 1 2) {subplotTitles = ["Line", "Bars"], sharedYAxes = ShareRows}
    & addTraceAt 1 1 myTrace
    & addTraceAt 1 2 (bar & #y ?~ dataArray [300, 900, 100 :: Double])
    & updateTracesOf @Bar mempty (#marker % #color ?~ One (Qualitative.d3 !! 1))
    & addHLine defaultSpanOptions {annotation = Just (#text ?~ "target")} (1000 :: Double)
    & Template.withTemplate Template.plotlyWhite
```

- **Subplots** (`Graphics.Plotly.Subplots`): `makeSubplots` lays out a grid
  as plotly.py's `make_subplots` does, with mixed subplot kinds (xy, 3D
  scenes, polar, geo, ternary, map, smith, and domain-based traces such as
  pies), row and column spans, shared axes, secondary y axes and titles.
  `addTraceAt row col` places a trace, and `printGrid` shows the grid.
- **Figure helpers** (`Graphics.Plotly.Figure`): select traces by type, cell,
  name or attribute, and update them, typed with `updateTracesOf @Scatter`
  or as traversals (`traces`, `tracesOf`); update the axes of some cells
  (`updateXAxes (inRow 2)`); add shapes and annotations, and lines and
  rectangles across subplots (`addHLine`, `addVLine`, `addHRect`,
  `addVRect`); build animation frames, play and pause buttons, and a frame
  slider.
- **Templates** (`Graphics.Plotly.Template`): plotly.py's ten templates,
  combined with `<>`. A figure has none unless you set one.
- **Colours** (`Graphics.Plotly.Colors`): plotly.py's qualitative,
  sequential, diverging and cyclical colour scales, with `toColorscale`,
  `sampleColorscale` and `nColors`.

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
`src-gen/`. The colour scales come from plotly.py (`vendor/plotly.py/`), along
with its templates. Regenerate the code with `cabal run plotlyhs-gen` after
updating either with `scripts/update-plotly-js.sh` or
`scripts/update-plotly-py.sh`.
