{-# LANGUAGE OverloadedStrings #-}

-- | Fails if the committed generated code differs from what the generator
-- produces now, or if the generator misreads the names a module declares.
-- The test runs in the package directory.
module Main (main) where

import Control.Monad (unless)
import Data.List (isPrefixOf)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
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
  [ name <> ": expected " <> expected <> ", got " <> actual
  | (name, expected, actual) <- samples <> rejections,
    expected /= actual
  ]
  where
    samples = case (,,,) <$> parse public <*> parse reexported <*> parse internal <*> parse generated of
      Left e -> [("sample modules", "no error", e)]
      Right (m, n, i, g) ->
        [ ("module name", "M", T.unpack (srcModule m)),
          ( "exports",
            show (Just [ExportType "A" True [], ExportType "B" False [], ExportType "C" False ["C1"], ExportModule "N", ExportType "G" False [], ExportType "H" True ["P"], ExportPattern "Q"]),
            show (srcExports m)
          ),
          ( "declarations",
            show ["A [A1,A2]", "B [B1]", "C [C1,C2]", "D [D1,D2]", "E []", "F []", "FA []", "G []", "H [H1,H2]", "I [I1]", "I [I2]" :: Text],
            show [t <> " [" <> T.intercalate "," cs <> "]" | (t, cs) <- srcDecls m]
          ),
          ("patterns", show ["P", "Q", "R" :: Text], show (srcPatterns m)),
          ( "reserved",
            show (Right ["A", "A1", "A2", "B", "C", "C1", "Ext", "G", "H", "H1", "H2", "N1", "P", "Q"] :: Either String [Text]),
            show (filter (`notElem` external) . Map.keys <$> reservedNames [m, n, i] [g])
          ),
          ("unknown re-export", "an error", refused (reservedNames [Source "U" (Just [ExportModule "Data.Map"]) [] []] [g])),
          ("unknown (..)", "an error", refused (reservedNames [Source "U" (Just [ExportType "Map" True []]) [] []] [g]))
        ]
    rejections =
      [ (name, "an error at " <> at, either (\e -> if (at <> ":") `isPrefixOf` e then "an error at " <> at else e) (const "no error") (parse src))
      | (name, at, src) <- rejected
      ]
    refused = either (const "an error") (const "no error")
    parse = parseSource "t.hs" . T.unlines
    external = concat [t : cs | (t, cs) <- externalTypes]
    public =
      [ "{-# LANGUAGE GADTs #-}",
        "{-# LANGUAGE CPP #-}",
        "",
        "-- | A module.",
        "module M",
        "  ( -- * Things",
        "    A (..),",
        "    B,",
        "    C (C1),",
        "    module N,",
        "    f,",
        "    type G,",
        "    H (.., P),",
        "    pattern Q,",
        "    (<+>),",
        "  )",
        "where",
        "",
        "import N",
        "",
        "{- data Hidden = Hidden",
        "   {- nested -} -}",
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
        "  type FA a",
        "  f :: a -> Int",
        "",
        "type G = A",
        "",
        "data H = forall a. (Show a) => H1 a | Int :+ Int | Int `H2` Int",
        "",
        "pattern P :: H",
        "pattern P = H1 '|'",
        "",
        "pattern Q, R :: H",
        "",
        "#if defined(X)",
        "data I = I1",
        "#else",
        "data I = I2",
        "#endif",
        "",
        "(<+>) :: Int -> Int -> Int",
        "x <+> y = x + y -- \"data NotHere\"",
        "",
        "s :: String",
        "s = \"-- {- data NotHere = N -}\""
      ]
    reexported = ["module N (N1 (..)) where", "", "data N1 = N1"]
    internal = ["module M.Internal.I (I (..)) where", "", "data I = I1"]
    generated = ["module Gen where", "", "data Ext = Ext"]
    -- Sources the scanner must refuse, and the line it must name.
    rejected =
      [ ("CPP definition", "t.hs:3", ["module M where", "", "#define X 1"]),
        ("unterminated #if", "t.hs:3", ["module M where", "", "#if X", "data A = A"]),
        ("CPP in the header", "t.hs:2", ["module M", "#if X", "  (A)", "#endif", "  where"]),
        ("unterminated comment", "t.hs:3", ["module M where", "", "{- data A = A"]),
        ("unterminated string", "t.hs:3", ["module M where", "", "s = \"abc"]),
        ("unreadable export", "t.hs:1", ["module M (A [x]) where"]),
        ("splice", "t.hs:3", ["module M where", "", "makeLenses ''A"]),
        ("$ splice", "t.hs:3", ["module M where", "", "$(deriveJSON ''A)"]),
        ("GADT constructor", "t.hs:4", ["module M where", "", "", "data T where", "  T1 Int"]),
        ("indented declarations", "t.hs:3", ["module M where", "", "  data A = A"]),
        ("no header", "t.hs:1", ["data A = A"])
      ]
