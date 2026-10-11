{-# LANGUAGE TemplateHaskell #-}

-- | The vendored plotly.js bundle, embedded in the library.
module Graphics.Plotly.Internal.Bundle (bundle) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

-- | @vendor/plotly.js/plotly.min.js@.
bundle :: ByteString
bundle = $(embedFile "vendor/plotly.js/plotly.min.js")
