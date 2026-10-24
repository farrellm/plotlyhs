{-# LANGUAGE OverloadedStrings #-}

-- | Fails if the committed generated code differs from what the generator
-- produces now, or if the generator misreads the names a module declares.
-- The test runs in the package directory.
module Main (main) where

import Control.Monad (unless)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import PlotlyGen
import PlotlyGen.Reserved
import System.Exit (exitFailure)
import System.FilePath ((</>))

main :: IO ()
main = do
  unless (null scannerFailures) $ do
    mapM_ (putStrLn . ("scanner: " <>)) scannerFailures
    exitFailure
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
          optColors = ".." </> optColors o,
          optTemplates = ".." </> optTemplates o,
          optSources = ".." </> optSources o,
          optOutput = ".." </> optOutput o,
          optCabal = ".." </> optCabal o
        }

-- | What the reserved-name scanner gets wrong on sample modules.
scannerFailures :: [String]
scannerFailures =
  [ name <> ": expected " <> show expected <> ", got " <> show actual
  | (name, expected, actual) <-
      [ ("module name", ["M"], [T.unpack (srcModule public)]),
        ("exports", ["A (..)", "B", "C (C1)", "module N", "f"], maybe [] (map T.unpack) (srcExports public)),
        ("declarations", ["A [A1,A2]", "B [B1]", "C [C1,C2]", "D [D1,D2]", "E []", "F []", "G []"], [T.unpack (t <> " [" <> T.intercalate "," cs <> "]") | (t, cs) <- srcDecls public]),
        ("reserved", ["A", "A1", "A2", "B", "C", "C1", "Ext", "N1"], [T.unpack n | n <- Map.keys reserved, n `notElem` external])
      ],
    expected /= actual
  ]
  where
    public =
      parseSource . T.unlines $
        [ "{-# LANGUAGE GADTs #-}",
          "",
          "-- | A module.",
          "module M",
          "  ( -- * Things",
          "    A (..),",
          "    B,",
          "    C (C1),",
          "    module N,",
          "    f,",
          "  )",
          "where",
          "",
          "import N",
          "",
          "-- | A.",
          "data A",
          "  = -- | The first.",
          "    A1 Int",
          "  | A2 {a2 :: Int}",
          "  deriving stock (Eq)",
          "",
          "newtype B (x :: Bool) = B1 Int",
          "",
          "data C = C1 | C2 (Maybe Int)",
          "",
          "data D (g :: Bool) where",
          "  D1 :: D 'True",
          "  D2 :: Int -> D 'False",
          "",
          "type family E (x :: Bool) :: Bool where",
          "  E x = x",
          "",
          "class (Show a) => F a where",
          "  f :: a -> Int",
          "",
          "type G = A",
          "",
          "f :: Int",
          "f = 1 -- not a declaration"
        ]
    reexported = parseSource "module N (N1 (..)) where\n\ndata N1 = N1\n"
    internal = parseSource "module M.Internal.I (I (..)) where\n\ndata I = I1\n"
    generated = parseSource "module Gen where\n\ndata Ext = Ext\n"
    reserved = reservedNames [public, reexported, internal] [generated]
    external = ["Bool", "Double", "Either", "Int", "Maybe", "Ordering", "Text", "Type", "Value", "Void", "False", "True", "Nothing", "Just", "Left", "Right"]
