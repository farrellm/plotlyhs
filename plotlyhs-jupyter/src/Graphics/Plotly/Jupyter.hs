{-# LANGUAGE OverloadedStrings #-}

-- |
-- Jupyter display support for plotlyhs. The display instance arrives with
-- Phase 2 of PLAN.md, once the core has its @Figure@ type.
module Graphics.Plotly.Jupyter (plotlyMimeType) where

import Data.Text (Text)

-- | The MIME type JupyterLab's plotly renderer accepts.
plotlyMimeType :: Text
plotlyMimeType = "application/vnd.plotly.v1+json"
