{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Embedding vendored files, checked at compile time.
module Graphics.Plotly.Internal.Embed (embedTemplate) where

import Data.Aeson (eitherDecodeStrict')
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.FileEmbed (bsToExp)
import Graphics.Plotly.Types (Template (..))
import Language.Haskell.TH (Code, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile, joinCode, unsafeCodeCoerce)

-- | A template from a JSON file, relative to the package, read when the
-- library is compiled: a file that is not a JSON object is a compile
-- error. The file is embedded as bytes and decoded on first use, which
-- keeps the compiled module small.
embedTemplate :: FilePath -> Code Q Template
embedTemplate path = joinCode $ do
  addDependentFile path
  bytes <- runIO (BS.readFile path)
  case decodeTemplate bytes of
    Left e -> fail ("cannot read the template " <> path <> ": " <> e)
    Right _ -> pure [||embedded path $$(unsafeCodeCoerce (bsToExp bytes))||]

decodeTemplate :: ByteString -> Either String Template
decodeTemplate = fmap Template . eitherDecodeStrict'

-- | Decode bytes that 'embedTemplate' has already checked.
embedded :: FilePath -> ByteString -> Template
embedded path = either (\e -> error ("embedded template " <> path <> ": " <> e)) id . decodeTemplate
