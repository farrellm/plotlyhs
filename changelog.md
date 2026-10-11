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

0.1.1

* Simple module to help build traces

* remove test-plotly executable


0.1

* Initial release
