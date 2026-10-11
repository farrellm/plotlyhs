-- |
--
-- Helper functions for defining valid JSON instances
module Graphics.Plotly.Utils where

import Data.Aeson.Types
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

unLens :: String -> String
unLens ('_' : s) = s
unLens s = s

dropInitial :: String -> String -> String
dropInitial s s' = fromMaybe s' (stripPrefix s s')

rename :: String -> String -> String -> String
rename froms tos s
  | s == froms = tos
  | otherwise = s

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { omitNothingFields = True,
      fieldLabelModifier = unLens
    }
