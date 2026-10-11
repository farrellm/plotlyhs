-- |
-- plotlyhs: Haskell bindings to plotly.js. This module re-exports the whole
-- library other than the HTML renderers.
module Graphics.Plotly
  ( module Graphics.Plotly.Core,
    module Graphics.Plotly.Types,
    module Graphics.Plotly.Enums,
    module Graphics.Plotly.Traces,
    module Graphics.Plotly.Layout,
  )
where

import Graphics.Plotly.Animation ()
import Graphics.Plotly.Config ()
import Graphics.Plotly.Core
import Graphics.Plotly.Enums
import Graphics.Plotly.Frame ()
import Graphics.Plotly.Layout
import Graphics.Plotly.Traces
import Graphics.Plotly.Types
