{-# LANGUAGE OverloadedStrings #-}

-- | Generates plotlyhs's typed modules from plotly.js's @plot-schema.json@.
module PlotlyGen
  ( Options (..),
    defaultOptions,
    generate,
    writeGenerated,
    checkGenerated,
  )
where

import Control.Monad (filterM, forM, forM_, unless)
import Data.Aeson (Value, eitherDecodeFileStrict')
import Data.List (sort, (\\))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import PlotlyGen.Naming
import PlotlyGen.Render
import PlotlyGen.Schema
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import System.FilePath (makeRelative, takeDirectory, takeExtension, (</>))

data Options = Options
  { -- | @plot-schema.json@; a @VERSION@ file beside it names the version.
    optSchema :: FilePath,
    -- | The directory the modules go in.
    optOutput :: FilePath,
    -- | The cabal file whose module list is kept up to date.
    optCabal :: FilePath
  }

-- | Paths relative to the repository root.
defaultOptions :: Options
defaultOptions =
  Options
    { optSchema = "vendor/plotly.js/plot-schema.json",
      optOutput = "src-gen",
      optCabal = "plotlyhs.cabal"
    }

-- | The generated modules, as file paths relative to the output directory
-- and their contents.
generate :: Text -> Value -> Either String [(FilePath, Text)]
generate version value = do
  schema <- readSchema value
  let traces = map traceName (schemaTraces schema)
  enums <- nameEnums traces [(path, leafType l) | (path, l) <- allLeaves schema]
  pure
    [ (T.unpack (T.replace "." "/" (moduleName m)) <> ".hs", moduleBody m)
    | m <- renderModules version schema enums
    ]

load :: Options -> IO [(FilePath, Text)]
load opts = do
  version <- T.strip <$> TIO.readFile (takeDirectory (optSchema opts) </> "VERSION")
  value <- either (fail . (("cannot read " <> optSchema opts <> ": ") <>)) pure =<< eitherDecodeFileStrict' (optSchema opts)
  either (fail . ("cannot generate: " <>)) pure (generate version value)

-- | Write the modules, delete stale ones, and update the cabal file's list.
writeGenerated :: Options -> IO ()
writeGenerated opts = do
  files <- load opts
  existing <- haskellFiles (optOutput opts)
  forM_ (existing \\ map fst files) $ \f -> removeFile (optOutput opts </> f)
  forM_ files $ \(f, body) -> do
    createDirectoryIfMissing True (takeDirectory (optOutput opts </> f))
    TIO.writeFile (optOutput opts </> f) body
  cabal <- TIO.readFile (optCabal opts)
  cabal' <- either fail pure (updateCabal files cabal)
  unless (cabal' == cabal) $ TIO.writeFile (optCabal opts) cabal'
  putStrLn ("wrote " <> show (length files) <> " modules to " <> optOutput opts)

-- | The files that differ from what would be generated, or that are stale.
checkGenerated :: Options -> IO [FilePath]
checkGenerated opts = do
  files <- load opts
  existing <- haskellFiles (optOutput opts)
  differing <- flip filterM files $ \(f, body) -> do
    let path = optOutput opts </> f
    exists <- doesFileExist path
    if exists then (/= body) <$> TIO.readFile path else pure True
  cabal <- TIO.readFile (optCabal opts)
  let cabalStale = either (const True) (/= cabal) (updateCabal files cabal)
  pure $
    map ((optOutput opts </>) . fst) differing
      <> map (optOutput opts </>) (existing \\ map fst files)
      <> [optCabal opts | cabalStale]

haskellFiles :: FilePath -> IO [FilePath]
haskellFiles root = do
  exists <- doesDirectoryExist root
  if exists then sort . map (makeRelative root) <$> go root else pure []
  where
    go dir = do
      entries <- map (dir </>) <$> listDirectory dir
      fmap concat . forM entries $ \e -> do
        isDir <- doesDirectoryExist e
        if isDir then go e else pure [e | takeExtension e == ".hs"]

-- | Replace the lines between the markers in the cabal file with the
-- generated modules.
updateCabal :: [(FilePath, Text)] -> Text -> Either String Text
updateCabal files cabal =
  case break (isMarker beginMarker) (T.lines cabal) of
    (before, begin : rest) -> case break (isMarker endMarker) rest of
      (_, end : after) ->
        let indent = T.takeWhile (== ' ') begin
         in Right (T.unlines (before <> [begin] <> map (indent <>) modules <> [end] <> after))
      _ -> Left ("no " <> T.unpack endMarker <> " line in the cabal file")
    _ -> Left ("no " <> T.unpack beginMarker <> " line in the cabal file")
  where
    isMarker m l = T.strip l == m
    modules = sort [T.replace "/" "." (T.pack (take (length f - 3) f)) | (f, _) <- files]
    beginMarker = "-- BEGIN GENERATED MODULES"
    endMarker = "-- END GENERATED MODULES"
