{-# LANGUAGE OverloadedStrings #-}

-- | The plotlyhs code generator. For now it only loads the schema and reports
-- what it contains; code generation arrives in Phase 1 of PLAN.md.
module Main (main) where

import Data.Aeson (Value (..), eitherDecodeFileStrict')
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Text qualified as T
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  path <-
    getArgs >>= \case
      [] -> pure "vendor/plotly.js/plot-schema.json"
      [p] -> pure p
      _ -> die "usage: plotlyhs-gen [PLOT-SCHEMA.JSON]"
  schema <- either (die . (("cannot read " <> path <> ": ") <>)) pure =<< eitherDecodeFileStrict' path
  traces <- maybe (die "schema has no traces object") pure (objectAt ["traces"] schema)
  let traceAttrs = [a | t <- KM.elems traces, Just a <- [objectAt ["attributes"] t]]
      layoutAttrs = objectAt ["layout", "layoutAttributes"] schema
  putStrLn $ "traces:            " <> show (KM.size traces)
  putStrLn $ "trace attributes:  " <> show (sum (map countAttributes traceAttrs))
  putStrLn $ "layout attributes: " <> show (maybe 0 countAttributes layoutAttrs)

-- | Follow a path of keys through nested objects.
objectAt :: [Key.Key] -> Value -> Maybe (KM.KeyMap Value)
objectAt [] (Object o) = Just o
objectAt (k : ks) (Object o) = KM.lookup k o >>= objectAt ks
objectAt _ _ = Nothing

-- | Count the attributes (objects with a @valType@) in an attribute tree.
-- Keys starting with @_@ are schema metadata, not attributes.
countAttributes :: KM.KeyMap Value -> Int
countAttributes o = sum [count v | (k, v) <- KM.toList o, not ("_" `T.isPrefixOf` Key.toText k)]
  where
    count (Object c)
      | KM.member "valType" c = 1
      | otherwise = countAttributes c
    count _ = 0
