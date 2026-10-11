# Modernization plan

Goal: full coverage of plotly.js (currently 4.1.2: 47 trace types, ~9,000
attributes), at parity with plotly.py and PlotlyBase.jl/PlotlyJS.jl, with a
typed Haskell API.

## Decisions

- **Core:** figures are JSON objects underneath, with phantom types on top.
  Typed optics are generated from plotly.js's `plot-schema.json`.
- **Optics, not microlens.** Use `optics-core`. Attributes are reached through
  `LabelOptic` instances, so user code reads
  `scatter & #x .~ xs & #marker % #color .~ red`.
- **A clean break, with no compatibility shims.** `Graphics.Plotly.Base`,
  `Simple`, `GoG` and `Histogram` are deleted, not deprecated. Releases are
  version tags on GitHub only; nothing is published to Hackage.
- **In scope:** static image export and Jupyter support (package `plotlyhs-jupyter`).

## Known bugs in 0.2.2 (the rewrite must not repeat them)

- `plotlyCDN` loads `plotly-latest.min.js`, which has been frozen at v1.58.5
  since 2021.
- `title` and axis titles are sent as strings, and the layout uses
  `titlefont`. plotly.js 3.0 removed both: a title must be an object,
  `{text, font, ...}`.
- `HoverMode`'s `False` constructor encodes as the string `"False"`. It also
  clashes with `Prelude.False` for anyone who imports `Graphics.Plotly`.
- Some attributes are sent under the wrong key and silently ignored:
  `sizeMode` (plotly expects `sizemode`), `annotataonbgcolor` (a typo), and a
  top-level `layout.zaxis` (plotly only accepts it inside `layout.scene`).
- GoG colours: `RGBA Double` produces `rgb(r,g,b,a)` with alpha scaled to 0–256.
  `RGB Double` scales by 256 instead of 255.
- Trace JSON is spliced into `<script>` unescaped, so a `</script>` in any label
  breaks the page and allows injection.
- `{displayModeBar: false}` is hard-coded, and there is no way to pass config.
- `hbox`/`vbox` set `mode` and `hcontour` sets `orientation`, attributes those
  traces don't have.
- `histogram` crashes on empty input and puts the maximum value in an extra bin.

## Phase 0 — Groundwork (done)

- Vendor `plot-schema.json` and `plotly.min.js` for plotly.js 4.1.2 under
  `vendor/plotly.js/`, with a script that updates both to a new version.
- Replace `microlens`/`microlens-th` with `optics-core`/`optics-th`.
- Add a tasty test suite with golden JSON tests, run in CI.
- Turn the repo into a multi-package `cabal.project` with three packages:
  - `plotlyhs` (core),
  - `plotlyhs-gen` (the code generator, not a dependency of the core),
  - `plotlyhs-jupyter`.

## Phase 1 — Generated core

- `Graphics.Plotly.Core`:
  - `newtype Trace (t :: TraceType) = Trace Object`,
    `newtype Obj (path :: Symbol) = Obj Object` for nested attribute objects
    (marker, line, axis, …), plus `Layout`, `Config`, `Frame`;
  - `data Figure = Figure { data_ :: [SomeTrace], layout :: Layout, config :: Config, frames :: [Frame] }`,
    which encodes to plotly's standard figure JSON so it can be exchanged with
    plotly.py (`pio.from_json`) and PlotlyBase.jl;
  - untyped escape hatches: `attr :: Text -> AffineTraversal' (Trace t) Value`
    takes a dotted path (`"marker.line.width"`), plus `rawTrace`.
- The generator (`plotlyhs-gen`) reads the schema and emits committed modules:
  - `Graphics.Plotly.Trace.<Name>`: a constructor (e.g. `scatter :: Trace Scatter`),
    `LabelOptic` instances for each attribute, and Haddock text from the
    schema's descriptions;
  - `Graphics.Plotly.Layout.*` for layout, axes, scenes, geo, polar, legend,
    colorbar and so on;
  - one Haskell type per schema value kind:

    | Schema value kind | Haskell type |
    |---|---|
    | enumerated | a sum type per distinct value set, deduplicated across traces |
    | flaglist | `Flags a` |
    | color | `Color` |
    | colorscale | `Colorscale` |
    | data_array | a `ToJSON` vector |
    | info_array | a tuple or list |
    | subplotid | `SubplotId` |
    | angle | `Angle` |
    | any | `Value` |
- Every optic is an `AffineTraversal`/`Lens` over `Maybe a`, so an unset field
  is omitted from the JSON.
- A golden test regenerates the code and fails if it differs from what is
  committed.
- Delete `Base`, `Simple`, `GoG` and `Histogram`.

## Phase 2 — Output

- `Graphics.Plotly.Html` takes an `HtmlOptions` record:
  - how to include plotly.js: `CDN version`, `Inline` (the vendored bundle,
    embedded), `Require`, or `None`;
  - `fullHtml`, a `divId` (generated automatically if not given), `config`,
    `responsive`.
- Escape `<`, `>`, `&`, U+2028 and U+2029 in the embedded JSON.
- The CDN link is pinned to the vendored version.
- `toHtml`, `writeHtml`, `toJson`, `writeJson`, `fromJson`, and
  `showInBrowser` (write a temporary file and open it with `xdg-open`/`open`).
- `Graphics.Plotly.Lucid` and `Graphics.Plotly.Blaze` become thin wrappers, as
  cabal sub-libraries so the core doesn't depend on lucid or blaze.
- Static images (`writeImage`: PNG, JPEG, SVG, PDF), in
  `Graphics.Plotly.Image`:
  - preferred: drive headless Chrome/Chromium directly. Render the HTML with
    the inline bundle, call `Plotly.toImage`, and capture the result through
    the DevTools protocol or `--dump-dom`. No Python needed.
  - fallback: call Python `kaleido>=1` if it is installed.
  - CI runs it with a Chromium installed on the runner.
- `plotlyhs-jupyter`: Jupyter display support (an IHaskell display instance) that outputs
  `application/vnd.plotly.v1+json`, with `text/html` as a fallback.

## Phase 3 — Figure helpers (plotly.py `Figure` / PlotlyBase.jl API)

- `makeSubplots` (rows, cols, specs, shared x/y axes, subplot titles,
  secondary y-axes, spacing), `addTraceAt row col`, and `printGrid`.
- `updateLayout`, `updateTraces` with a selector, and `forEachTrace`.
- `addHLine`, `addVLine`, `addHRect`, `addVRect`, shapes and annotations.
- Frames, plus animation settings, sliders and updatemenus helpers.
- Templates: bundle plotly.py's 10 template JSON files (plotly, plotly_white,
  plotly_dark, ggplot2, seaborn, simple_white, presentation, gridon, xgridoff,
  ygridoff) and add a `Template` type with merge support.
- `Graphics.Plotly.Colors`: qualitative, sequential and diverging colour
  scales, matching `plotly.colors`.

## Phase 4 — Express-style API (`Graphics.Plotly.Express`)

- Builders over `[a]` with accessor functions (e.g. `scatterOf`, `lineOf`,
  `barOf`), each returning a `Figure`:

  | Category | Builders |
  |---|---|
  | Basic | `scatterOf`, `lineOf`, `areaOf`, `barOf`, `pieOf`, `funnelOf` |
  | Distributions | `histogramOf` (native `histogram` trace), `boxOf`, `violinOf`, `ecdfOf`, `stripOf` |
  | 2D and images | `densityHeatmapOf`, `densityContourOf`, `imshow` |
  | Specialized | `scatter3dOf`, `scatterPolarOf`, `parallelCoordinatesOf`, `sunburstOf`, `treemapOf`, `timelineOf`, `scatterMapOf`/`choroplethMapOf` |
- An options record covering colour, symbol, size, facet_row/facet_col/facet_wrap,
  hover data, labels, template and marginals.
- Grouping by colour or symbol produces one trace per group, with legend groups.
  Faceting builds on Phase 3's subplots.

## Phase 5 — Quality, docs, release

- In the tests, validate the emitted JSON against the schema (keys, enum
  values, value types) in Haskell. In CI, also run plotly.js's
  `Plotly.validate` under Node.
- Add an `examples/` gallery executable with at least one example per trace
  type and per Express function. It generates `docs/index.html`, and CI checks
  that the committed copy is current.
- Haddock coverage target: 100%, since descriptions come from the schema.
- Test GHC 9.6, 9.8, 9.10 and 9.12 in a CI matrix, with base bounds widened to
  match.
- README, changelog, and tag v1.0.0 on GitHub.
