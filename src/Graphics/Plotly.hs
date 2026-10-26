-- |
-- plotlyhs: Haskell bindings to plotly.js. This module re-exports the whole
-- library other than its output ("Graphics.Plotly.Html",
-- "Graphics.Plotly.Json" and "Graphics.Plotly.Image") and the modules
-- meant to be imported qualified ("Graphics.Plotly.Template" and
-- "Graphics.Plotly.Colors").
module Graphics.Plotly
  ( module Graphics.Plotly.Core,
    module Graphics.Plotly.Types,
    module Graphics.Plotly.Enums,
    module Graphics.Plotly.Traces,
    module Graphics.Plotly.Layout,
    module Graphics.Plotly.Subplots,
    module Graphics.Plotly.Figure,
  )
where

import Graphics.Plotly.Animation ()
import Graphics.Plotly.Config ()
import Graphics.Plotly.Core
import Graphics.Plotly.Enums
import Graphics.Plotly.Figure
import Graphics.Plotly.Frame ()
import Graphics.Plotly.Layout
-- The helpers the checked functions are built from stay in
-- "Graphics.Plotly.Grid", out of the way of names like Hedgehog's check.
import Graphics.Plotly.Subplots hiding (check, natInt)
import Graphics.Plotly.Traces
import Graphics.Plotly.Types
