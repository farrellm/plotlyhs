Unreleased

* Build with cabal on GHC 9.12 only; drop stack, nix and Travis CI for GitHub Actions
* Formatted with ormolu; hlint clean
* Use optics (optics-core, optics-th) instead of microlens; compose with `%`, not `.`
* Golden test suite (tasty) for the JSON and HTML output
* Multi-package cabal.project: plotlyhs, plotlyhs-gen (schema-driven code generator) and plotlyhs-jupyter
* Vendor plotly.js 4.1.2 (bundle and plot-schema.json) under vendor/plotly.js, updated by scripts/update-plotly-js.sh
* Breaking: a new core generated from plotly.js's schema, covering every trace type and attribute.
  Figures, traces and nested objects are JSON objects with phantom types (`Figure`, `Trace t`, `Obj path`),
  and attributes are optics named by labels (`#marker % #color`)
* Breaking: `Graphics.Plotly.Base`, `Simple`, `GoG`, `Histogram` and `Utils` are removed
* Breaking: `Graphics.Plotly.Lucid` and `Blaze` render a `Figure`, with `plotly divId figure`;
  `plotlyCDN` loads the vendored plotly.js version instead of the frozen `plotly-latest`
* `Graphics.Plotly.Html`: `toHtml`, `writeHtml` and `showInBrowser`, with `HtmlOptions`
  (plotly.js from the CDN, inline, loaded dynamically or not at all; a whole page or a fragment;
  the div id; responsive sizing). The embedded JSON also escapes U+2028 and U+2029
* `Graphics.Plotly.Json`: `toJson`, `fromJson`, `writeJson` and `readJson`
* `Graphics.Plotly.Image`: `writeImage` and `toImage` (PNG, JPEG, WebP, SVG, PDF), drawn by
  plotly.js in headless Chrome, or by Python's kaleido when there is no Chrome
* Breaking: `Graphics.Plotly.Lucid` and `Blaze` move to the `plotlyhs:lucid` and `plotlyhs:blaze`
  sub-libraries, and gain `plotlyWith`
* plotlyhs-jupyter: IHaskell display instances for `Figure` and `Trace t`
* Breaking: `Figure` is indexed by its subplot grid, `Figure (g :: FigureGrid)`. A plain figure is
  `Figure 'NoGrid`, and its constructor is hidden: build one with `figure`, and reach its parts with
  `#data`, `#layout`, `#config` and `#frames`. `fromJson` reads a `Figure 'NoGrid`
* `Graphics.Plotly.Subplots` and `Graphics.Plotly.Grid`: grids of subplots as types
  (`'[ '[ 'Span 1 2 XY, 'Blank], '[PlotY2, Domain]]`), laid out by `makeSubplots` as plotly.py's
  `make_subplots` does. Traces go in with `addTraceAt @row @col` and `addTraceSecondaryY`. These
  are compile errors with plotlyhs messages: a cell outside the grid or blank, a trace on the wrong
  kind of subplot, a missing secondary y axis, and a malformed grid (ragged, spans overlapping or
  past the edge). `withUniformSubplots` makes grids sized at runtime, and `printGrid` matches
  plotly.py's `print_grid`. Spacing, padding, widths and heights cannot fail: they are clamped
* `Graphics.Plotly.Figure`: trace selectors and traversals (`traces`, `tracesOf`, `updateTraces`,
  `updateTracesOf`, `forEachTrace`), `updateLayout`, `updateXAxes` and `updateYAxes` by cell
  (`inRow @2`, `atCell @1 @2`, checked against the grid at compile time; `primaryY` and
  `secondaryY` pick a y axis, and a `cellY2` index selects only the secondary one),
  shapes and annotations (`addShape`, `addHLine`, `addVRect`, …), and animation helpers
  (`frame`, `playPauseMenu`, `framesSlider`)
* `Graphics.Plotly.Template`: plotly.py's ten templates, and a `Template` type that merges with `<>`;
  `layout.template` is typed as a `Template`
* `Graphics.Plotly.Colors`: plotly.py's colour scales (qualitative, sequential, diverging, cyclical),
  with `toColorscale`, `sampleColorscale` and `nColors`
* Vendor plotly.py 7.1.0's templates and colour scales under vendor/plotly.py, updated by
  scripts/update-plotly-py.sh
* Breaking: the `Shape` enumeration (marker pattern shapes) is now `FillpatternShape`; `Shape` is
  the type of a layout shape
* Breaking: cartesian axes are typed by their dimension. A trace's `xaxis` and `yaxis` are
  `AxisId 'X` and `AxisId 'Y` (`#xaxis ?~ AxisId 2` for `"x2"`), and the `xref` and `yref` of
  shapes, annotations, images and selections are `AxisRef 'X` and `AxisRef 'Y`: `Paper`, `Axis`
  or `AxisDomain` (`"x2 domain"`), replacing `AnnotationXref`. An x reference cannot be given
  where a y one is meant. An `xref` or `yref` that is not a valid id (`"y1"`) is read as unset,
  as plotly.js reads it: `addShapeAt` and friends place such a shape on the cell's axes, and
  `addShape` on the first ones

0.1.1

* Simple module to help build traces

* remove test-plotly executable


0.1

* Initial release
