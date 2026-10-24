{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- plotly.py's templates, vendored from plotly.py and embedded in the
-- library. Import this module qualified:
--
-- @
-- import Graphics.Plotly.Template qualified as Template
--
-- dark :: Figure g -> Figure g
-- dark = Template.withTemplate (Template.plotlyDark <> Template.presentation)
-- @
--
-- Templates combine with '<>', the right one winning (see 'Template').
-- A figure has no template unless one is set, so it looks like plain
-- plotly.js; plotly.py's figures use 'plotly' by default.
module Graphics.Plotly.Template
  ( Template (..),
    withTemplate,
    named,
    names,

    -- * plotly.py's templates
    module Graphics.Plotly.Template.Builtin,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Plotly.Core (Figure)
import Graphics.Plotly.Layout ()
import Graphics.Plotly.Template.Builtin
import Graphics.Plotly.Types (Template (..))
import Optics.Core

-- | Set the figure's template, replacing any it has.
withTemplate :: Template -> Figure g -> Figure g
withTemplate t = #layout % #template ?~ t

-- | A template by its plotly.py name (@"plotly_dark"@), or several joined
-- with @+@ (@"plotly_dark+presentation"@), merged left to right.
named :: Text -> Maybe Template
named n = mconcat <$> traverse (`lookup` templates) (T.splitOn "+" n)

-- | The plotly.py names of the templates.
names :: [Text]
names = map fst templates
