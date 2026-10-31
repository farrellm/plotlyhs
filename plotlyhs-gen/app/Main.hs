-- | The plotlyhs code generator. Run it from the repository root.
module Main (main) where

import PlotlyGen
import System.Environment (getArgs)
import System.Exit (die, exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main =
  getArgs >>= \case
    [] -> writeGenerated =<< generateModules defaultOptions
    ["--check"] -> do
      stale <- checkGenerated =<< generateModules defaultOptions
      if null stale
        then putStrLn "generated code is up to date"
        else do
          mapM_ (hPutStrLn stderr . ("out of date: " <>)) stale
          hPutStrLn stderr "run `cabal run plotlyhs-gen` from the repository root"
          exitFailure
    _ -> die "usage: plotlyhs-gen [--check]"
