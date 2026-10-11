-- | Fails if the committed generated code differs from what the generator
-- produces now. The test runs in the package directory.
module Main (main) where

import PlotlyGen
import System.Exit (exitFailure)
import System.FilePath ((</>))

main :: IO ()
main = do
  stale <- checkGenerated (inRoot defaultOptions)
  if null stale
    then putStrLn "generated code is up to date"
    else do
      mapM_ (putStrLn . ("out of date: " <>)) stale
      putStrLn "run `cabal run plotlyhs-gen` from the repository root"
      exitFailure
  where
    inRoot o =
      o
        { optSchema = ".." </> optSchema o,
          optOutput = ".." </> optOutput o,
          optCabal = ".." </> optCabal o
        }
