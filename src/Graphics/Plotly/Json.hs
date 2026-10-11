-- |
-- Figures as JSON, in plotly.js's figure format: @{data, layout, config,
-- frames}@. plotly.py reads it with @plotly.io.from_json@ and writes it with
-- @plotly.io.to_json@, and PlotlyBase.jl reads and writes it too.
module Graphics.Plotly.Json
  ( toJson,
    fromJson,
    writeJson,
    readJson,
  )
where

import Data.Aeson (eitherDecode, eitherDecodeFileStrict, encode, encodeFile)
import Data.ByteString.Lazy (ByteString)
import Graphics.Plotly.Core (Figure)

-- | The figure's JSON.
toJson :: Figure -> ByteString
toJson = encode

-- | A figure from its JSON, or why it is not one.
fromJson :: ByteString -> Either String Figure
fromJson = eitherDecode

-- | Write the figure's JSON to a file.
writeJson :: FilePath -> Figure -> IO ()
writeJson = encodeFile

-- | Read a figure from a JSON file, or why it is not one.
readJson :: FilePath -> IO (Either String Figure)
readJson = eitherDecodeFileStrict
