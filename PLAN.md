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

## Phase 1 — Generated core (done)

- `Graphics.Plotly.Core`:
  - `newtype Trace (t :: TraceType) = Trace Object`, and
    `newtype Obj (path :: Symbol) = Obj Object` for nested attribute objects
    (`Obj "scatter.marker"`, `Obj "layout.xaxis"`, …). `Layout`, `Config`,
    `Frame` and `Animation` are `Obj "layout"` and so on;
  - `data Figure = Figure { data_ :: [SomeTrace], layout :: Layout, config :: Config, frames :: [Frame] }`,
    which encodes to plotly's standard figure JSON so it can be exchanged with
    plotly.py (`pio.from_json`) and PlotlyBase.jl;
  - `SomeTrace` is an untyped trace; `fromSomeTrace` recovers a typed one;
  - untyped escape hatches: `attr :: IsObject s => Text -> Lens' s (Maybe Value)`
    takes a dotted path (`"marker.line.width"`), plus `rawTrace`.
- The generator (`plotlyhs-gen`) reads the schema and emits committed modules
  under `src-gen/`, and keeps the cabal file's module list current:
  - `Graphics.Plotly.Trace.<Name>`: a constructor (e.g. `scatter :: Trace Scatter`),
    `LabelOptic` instances for each attribute of the trace and its nested
    objects, and Haddock text from the schema's descriptions;
  - `Graphics.Plotly.Layout` and `Graphics.Plotly.Layout.*` (one module per
    large layout object: axes, scenes, geo, polar, legend, …), with
    `xaxisN :: Int -> Lens' Layout (Obj "layout.xaxis")` and the like for
    numbered subplots; trace types' layout attributes (`barmode`, …) are merged
    in;
  - `Graphics.Plotly.Config`, `Frame`, `Animation`, `TraceType`, `Version`;
  - one Haskell type per schema value kind:

    | Schema value kind | Haskell type |
    |---|---|
    | enumerated | a sum type per distinct value set, deduplicated across the schema, in `Graphics.Plotly.Enums` |
    | flaglist | `Flags a`, with a generated flag type |
    | color | `Color` |
    | colorlist | `[Color]` |
    | colorscale | `Colorscale` |
    | data_array | `DataArray` (built with `dataArray`) |
    | info_array | a tuple, or a list |
    | subplotid | `SubplotId` |
    | angle | `Angle` |
    | any | `Value` |
    | arrayOk | `ArrayOk a` (`One a` or `Many [a]`) |
- Every attribute's optic is a `Lens'` onto `Maybe a`, so an unset field is
  omitted from the JSON. A nested object's optic is a `Lens'` onto the object,
  empty when unset, so optics compose: `#marker % #line % #width`.
- A test (`plotlyhs-gen`'s `generated` suite, and `plotlyhs-gen --check`)
  regenerates the code and fails if it differs from what is committed.
- `Base`, `Simple`, `GoG`, `Histogram` and `Utils` are deleted. `Lucid` and
  `Blaze` render a `Figure` until Phase 2 replaces them.

## Phase 2 — Output (done)

- `Graphics.Plotly.Html` takes an `HtmlOptions` record:
  - how to include plotly.js: `CDN version`, `Inline` (the vendored bundle,
    embedded with `file-embed`), `Dynamic`, or `None`. `Dynamic` replaces the
    planned `Require`: plotly.js 3 and later are not AMD modules (they always
    set `window.Plotly`), so it loads the CDN bundle with a `script` tag, once
    per page, unless the page already has plotly.js;
  - `fullHtml`, a `divId` (by default a hash of the figure's JSON, so the
    output is deterministic), and `responsive`. There is no `config` option:
    the figure carries its own.
- `<`, `>`, `&`, U+2028 and U+2029 are escaped in the embedded JSON.
- The CDN link is pinned to the vendored version.
- `toHtml`, `writeHtml`, `showInBrowser` in `Graphics.Plotly.Html`; `toJson`,
  `fromJson`, `writeJson`, `readJson` in `Graphics.Plotly.Json`. Neither
  module is re-exported by `Graphics.Plotly` (`toHtml` would clash with
  lucid's and blaze's).
- `Graphics.Plotly.Lucid` and `Graphics.Plotly.Blaze` are thin wrappers, in
  the public sub-libraries `plotlyhs:lucid` and `plotlyhs:blaze`.
- Static images in `Graphics.Plotly.Image` (`writeImage`, `toImage`: PNG,
  JPEG, WebP, SVG, PDF):
  - headless Chrome/Chromium, run with `--dump-dom`: the page calls
    `Plotly.toImage` and writes the result, base64-encoded, into the DOM. A
    PDF is drawn as SVG and printed with `--print-to-pdf`. No Python needed;
  - fallback: Python `kaleido>=1`, given the vendored bundle;
  - CI runs the image tests with the runner's preinstalled Chrome.
- `plotlyhs-jupyter`: IHaskell display instances for `Figure` and `Trace t`,
  as `text/html` (the `Dynamic` loader) with `text/plain`. The
  `application/vnd.plotly.v1+json` output is blocked upstream: IHaskell
  (0.13 and master) sends a custom MIME type's data as a JSON string, and
  plotly's renderers need an object. IHaskell's `use-hlint` flag is off in
  `cabal.project`, since hlint does not yet allow aeson 2.3.

## Phase 3 — Figure helpers (done)

- **Grids as types.** `Figure (g :: FigureGrid)` is `'NoGrid`, or `'Grid` of
  rows of `CellSpec`s (`'Plot kind`, `PlotY2`, `'Span rows cols spec`,
  `'Blank`) (`Graphics.Plotly.Grid`).
  - Misuse is a compile error with a plotlyhs message, never a runtime
    `error`: a cell outside the grid or blank, a trace whose kind does not
    match the cell's (or that has no subplot of its own, like `splom`), a
    missing secondary y axis, a malformed grid (ragged, empty, spans
    overlapping or past the edge), or a row or column outside the grid when
    selecting cells.
  - Each check is a type family that is an equality with `'()`. `check`
    uses the equality's evidence, so the check is not a redundant constraint
    and deferred type errors fire, which `test/TypeErrors.hs` relies on.
  - `TraceSubplotKind` and `TraceName` are generated from the schema. A
    `SubplotKind` is `SubplotXY`, `SubplotDomain`, or `SubplotNamed` of a
    `NamedKind` (scene, polar, …), so functions on named kinds are total;
    `splom` has no kind (`'Nothing`). Everything about a named kind (its
    layout key, trace attribute, cell type and docs) comes from one table
    in the generator, `namedKindDefs`, which emits
    `Graphics.Plotly.SubplotKind`: the kinds, `CellSpec` and its synonyms
    (`XY`, `Scene`, …), `KindName`, and `KnownSubplotKind`.
  - The constructor of `Figure` is hidden, so a typed figure always has the
    layout its type describes.
- `Graphics.Plotly.Subplots`: `makeSubplots @g subplotOptions`, with
  shared x/y axes, start cell, spacing, column widths and row heights (as
  functions of the column or row number), cell padding, and
  subplot/column/row/x/y titles.
  - It reproduces plotly.py's `make_subplots` layout exactly;
    `scripts/check-figures.py` compares the goldens with plotly.py's output.
  - Options cannot fail: spacing and padding are clamped.
  - Insets are not supported. Smith charts are, although plotly.py's
    `make_subplots` does not support them.
  - `addTraceAt @r @c` and `addTraceSecondaryY @r @c` place a trace.
  - `CellIx` values (`cell @r @c`, `cellY2`) and `addTraceIn` place traces
    by cell values. `withUniformSubplots` uses them for grids sized at
    runtime, which Phase 4's faceting will use.
  - `printGrid` matches plotly.py's `print_grid`.
- `Graphics.Plotly.Figure`:
  - **Selecting traces:** a `Selector g` is a predicate (`ofType @t`,
    `atIndex`, `named`, `inCells`, `onSecondaryY`, `withAttr`, `where_`),
    combined with `<>`. `traces` and `tracesOf @t` are traversals.
    `updateTraces`, `updateTracesOf`, `forEachTrace` and `selectTraces`
    wrap them.
  - **Axes:** `updateLayout`, plus `updateXAxes`/`updateYAxes` (and the
    `xaxes`/`yaxes` traversals), which take `Cells g` (`allCells`,
    `inRow @r`, `inCol @c`, `atCell @r @c`, `cellsOf`), checked against the
    grid.
  - **Shapes and annotations:** `addShape`, `addAnnotation` and their `...At`
    forms. `addHLine`, `addVLine`, `addHRect` and `addVRect` take
    `SpanOptions` and follow plotly.py's `add_hline` family, including
    annotation positions (an `AnnotationPosition` type) and skipping empty
    subplots.
  - **Animation:** `frame`, `addFrames`, `animationButton`,
    `playPauseMenu` and `framesSlider`.
- Templates: plotly.py 7.1.0's 10 template JSON files are vendored under
  `vendor/plotly.py/templates/` (by `scripts/update-plotly-py.sh`) and
  embedded in `Graphics.Plotly.Template`.
  - `Template` merges with `<>`, as plotly.py's `merge_templates` does.
  - The generator types `layout.template` as `Template`.
  - No template is applied by default.
- `Graphics.Plotly.Colors`: plotly.py's colour scales are extracted (with
  Python's `ast`, without importing plotly) to `vendor/plotly.py/colors.json`.
  - The generator emits `Graphics.Plotly.Colors.{Qualitative,Sequential,Diverging,Cyclical}`
    from that file.
  - Helpers: `toColorscale`, `namedColorscale`, `sampleColorscale` and
    `nColors`.
- Names that the core and the new modules export (`Shape`, `Template`,
  `Grid`, …) are reserved in the generator. As a result, the old `Shape`
  enumeration is now `FillpatternShape`.

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
  Faceting builds on Phase 3's subplots, through `withUniformSubplots` (grids sized at runtime).

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
