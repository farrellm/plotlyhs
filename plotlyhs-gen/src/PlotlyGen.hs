{-# LANGUAGE OverloadedStrings #-}

-- | Generates plotlyhs's typed modules from plotly.js's @plot-schema.json@.
module PlotlyGen
  ( Options (..),
    defaultOptions,
    Inputs (..),
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
import PlotlyGen.Colors
import PlotlyGen.Naming
import PlotlyGen.Render
import PlotlyGen.Reserved
import PlotlyGen.Schema
import PlotlyGen.Templates
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import System.FilePath (makeRelative, takeBaseName, takeDirectory, takeExtension, (</>))

data Options = Options
  { -- | @plot-schema.json@; a @VERSION@ file beside it names the version.
    optSchema :: FilePath,
    -- | plotly.py's colour scales, @colors.json@; a @VERSION@ file beside it
    -- names plotly.py's version.
    optColors :: FilePath,
    -- | plotly.py's templates, a directory of JSON files.
    optTemplates :: FilePath,
    -- | The hand-written modules, whose exports the enumerations' names
    -- must not clash with.
    optSources :: FilePath,
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
      optColors = "vendor/plotly.py/colors.json",
      optTemplates = "vendor/plotly.py/templates",
      optSources = "src",
      optOutput = "src-gen",
      optCabal = "plotlyhs.cabal"
    }

-- | What the generator reads.
data Inputs = Inputs
  { -- | plotly.js's version and schema.
    inSchema :: (Text, Value),
    -- | plotly.py's version and colour scales.
    inColors :: (Text, Value),
    -- | plotly.py's templates, by name.
    inTemplates :: [(Text, Value)],
    -- | The hand-written modules' source.
    inSources :: [Text]
  }

-- | The generated modules, as file paths relative to the output directory
-- and their contents, and notes on enumerations renamed so as not to clash
-- with other names.
generate :: Inputs -> Either String ([(FilePath, Text)], [Text])
generate inputs = do
  let (version, value) = inSchema inputs
      (pyVersion, colors) = inColors inputs
  schema <- readSchema value
  colorModules <- renderColorModules pyVersion colors
  templateModule <- renderTemplateModule pyVersion (inTemplates inputs)
  let uses = [(path, leafType l) | (path, l) <- allLeaves schema]
      fixed = colorModules <> [templateModule]
      others = filter ((/= enumsModuleName) . moduleName) (renderModules version schema (placeholderEnums uses)) <> fixed
      reserved = reservedNames (map parseSource (inSources inputs)) (map (parseSource . moduleBody) others)
  (enums, notes) <- nameEnums reserved uses
  pure
    ( [ (T.unpack (T.replace "." "/" (moduleName m)) <> ".hs", moduleBody m)
      | m <- renderModules version schema enums <> fixed
      ],
      notes
    )

load :: Options -> IO [(FilePath, Text)]
load opts = do
  schema <- input (optSchema opts)
  colors <- input (optColors opts)
  files <- sort . filter ((== ".json") . takeExtension) <$> listDirectory (optTemplates opts)
  templates <- forM files $ \f -> (,) (T.pack (takeBaseName f)) <$> decodeFile (optTemplates opts </> f)
  sources <- mapM (TIO.readFile . (optSources opts </>)) =<< haskellFiles (optSources opts)
  (modules, notes) <- either (fail . ("cannot generate: " <>)) pure (generate (Inputs schema colors templates sources))
  mapM_ (TIO.putStrLn . ("note: " <>)) notes
  pure modules
  where
    input path = do
      version <- T.strip <$> TIO.readFile (takeDirectory path </> "VERSION")
      value <- decodeFile path
      pure (version, value)
    decodeFile path = either (fail . (("cannot read " <> path <> ": ") <>)) pure =<< eitherDecodeFileStrict' path

-- | Write the modules, delete stale ones, and update the cabal file's list.
writeGenerated :: Options -> IO ()
writeGenerated opts = do
  files <- load opts
  existing <- haskellFiles (optOutput opts)
  forM_ (existing \\ map fst files) $ \f -> removeFile (optOutput opts </> f)
  -- Unchanged files keep their timestamps, so cabal rebuilds only what changed.
  forM_ files $ \(f, body) -> do
    let path = optOutput opts </> f
    current <- readIfExists path
    unless (current == Just body) $ do
      createDirectoryIfMissing True (takeDirectory path)
      TIO.writeFile path body
  cabal <- TIO.readFile (optCabal opts)
  cabal' <- either fail pure (updateCabal files cabal)
  unless (cabal' == cabal) $ TIO.writeFile (optCabal opts) cabal'
  putStrLn ("generated " <> show (length files) <> " modules in " <> optOutput opts)

-- | The files that differ from what would be generated, or that are stale.
checkGenerated :: Options -> IO [FilePath]
checkGenerated opts = do
  files <- load opts
  existing <- haskellFiles (optOutput opts)
  differing <- flip filterM files $ \(f, body) ->
    (/= Just body) <$> readIfExists (optOutput opts </> f)
  cabal <- TIO.readFile (optCabal opts)
  let cabalStale = either (const True) (/= cabal) (updateCabal files cabal)
  pure $
    map ((optOutput opts </>) . fst) differing
      <> map (optOutput opts </>) (existing \\ map fst files)
      <> [optCabal opts | cabalStale]

readIfExists :: FilePath -> IO (Maybe Text)
readIfExists path = do
  exists <- doesFileExist path
  if exists then Just <$> TIO.readFile path else pure Nothing

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
