-- |
--
-- Re-exports the Simple interface, the grammar of grpahics
-- interface and parts of the base interface.
module Graphics.Plotly
  ( module Base,
    module Simple,
    module GoG,
  )
where

import Graphics.Plotly.Base as Base hiding
  ( line,
    size,
    x,
    y,
    z,
    _line,
    _size,
    _x,
    _y,
    _z,
  )
import Graphics.Plotly.GoG as GoG
import Graphics.Plotly.Simple as Simple
