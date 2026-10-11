Unreleased

* Build with cabal on GHC 9.12 only; drop stack, nix and Travis CI for GitHub Actions
* Formatted with ormolu; hlint clean
* Use optics (optics-core, optics-th) instead of microlens; compose with `%`, not `.`
* Golden test suite (tasty) for the JSON and HTML output
* Multi-package cabal.project: plotlyhs, plotlyhs-gen (schema-driven code generator) and plotlyhs-jupyter
* Vendor plotly.js 4.1.2 (bundle and plot-schema.json) under vendor/plotly.js, updated by scripts/update-plotly-js.sh

0.1.1

* Simple module to help build traces

* remove test-plotly executable


0.1

* Initial release
