{-# LANGUAGE OverloadedStrings #-}

-- | Golden tests of JSON, in the format of the files in @test/golden@.
module Golden (goldenJson, prettyJson) where

import Data.Aeson (ToJSON, Value, toJSON)
import Data.Aeson.Encode.Pretty (Config (..), Indent (..), defConfig, encodePretty')
import Data.ByteString.Lazy qualified as BL
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

-- | A test that the value's JSON matches @test/golden/<file>.json@.
goldenJson :: (ToJSON a) => String -> a -> TestTree
goldenJson file v =
  goldenVsString file ("test/golden/" <> file <> ".json") (pure (prettyJson (toJSON v)))

prettyJson :: Value -> BL.ByteString
prettyJson v = encodePretty' defConfig {confIndent = Spaces 2, confCompare = compare} v <> "\n"
