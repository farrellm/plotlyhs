-- |
-- Figures, traces and the objects inside them. Each is a JSON object
-- underneath, with a phantom type that says which attributes it has. The
-- attributes are reached through optics named by labels, generated from
-- plotly.js's schema. With @OverloadedLabels@ and @OverloadedStrings@:
--
-- @
-- import Graphics.Plotly
-- import Optics.Core
--
-- fig :: Figure 'NoGrid
-- fig =
--   figure [someTrace points]
--     & #layout % #title % #text ?~ "Points"
--
-- points :: Trace Scatter
-- points =
--   scatter
--     & #x ?~ dataArray [1, 2, 3 :: Double]
--     & #y ?~ dataArray [2, 4, 3 :: Double]
--     & #mode ?~ Flags (ModeLines :| [ModeMarkers])
--     & #marker % #color ?~ \"red\"
-- @
--
-- An attribute's optic is a 'Optics.Core.Lens'' onto 'Maybe' its value, and an unset
-- attribute is left out of the JSON. A nested object's optic is a 'Optics.Core.Lens''
-- onto the object itself, which is empty when unset.
--
-- A figure's traces, layout, configuration and frames are reached with
-- the optics @#data@, @#layout@, @#config@ and @#frames@.
module Graphics.Plotly.Core
  ( -- * Figures
    Figure,
    figure,
    forgetGrid,
    FigureGrid (..),
    StartCell (..),

    -- * Traces
    Trace (..),
    newTrace,
    TraceType (..),
    KnownTraceType (..),
    traceTypeName,
    traceTypeFromName,
    SubplotKind (..),
    traceSubplotKind,

    -- ** Traces of any type
    SomeTrace (..),
    someTrace,
    someTraceType,
    fromSomeTrace,
    rawTrace,

    -- * Objects
    Obj (..),
    emptyObj,
    Layout,
    Config,
    Frame,
    Animation,
    XAxis,
    YAxis,
    Shape,
    Annotation,
    Slider,
    SliderStep,
    Updatemenu,
    Button,

    -- * Untyped access
    IsObject (..),
    attr,

    -- * Building optics
    field,
    nested,
    subplotKey,
  )
where

import Graphics.Plotly.Internal.Core
