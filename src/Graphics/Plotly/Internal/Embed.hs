{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Embedding vendored files, checked at compile time.
module Graphics.Plotly.Internal.Embed (embedTemplate) where

import Data.Aeson (Object, eitherDecodeFileStrict')
import Graphics.Plotly.Types (Template (..))
import Language.Haskell.TH (Code, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, joinCode)

-- | A template from a JSON file, relative to the package, read when the
-- library is compiled: a file that is not a JSON object is a compile
-- error.
embedTemplate :: FilePath -> Code Q Template
embedTemplate path = joinCode $ do
  addDependentFile path
  runIO (eitherDecodeFileStrict' path) >>= \case
    Left e -> fail ("cannot read the template " <> path <> ": " <> e)
    Right (o :: Object) -> pure [||Template o||]
