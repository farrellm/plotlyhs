{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
    plotly,
    plotlyWhite,
    plotlyDark,
    ggplot2,
    seaborn,
    simpleWhite,
    presentation,
    gridon,
    xgridoff,
    ygridoff,
  )
where

import Data.Aeson (eitherDecodeStrict)
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Plotly.Core (Figure)
import Graphics.Plotly.Layout ()
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

templates :: [(Text, Template)]
templates =
  [ ("plotly", plotly),
    ("plotly_white", plotlyWhite),
    ("plotly_dark", plotlyDark),
    ("ggplot2", ggplot2),
    ("seaborn", seaborn),
    ("simple_white", simpleWhite),
    ("presentation", presentation),
    ("gridon", gridon),
    ("xgridoff", xgridoff),
    ("ygridoff", ygridoff)
  ]

decode :: String -> ByteString -> Template
decode name = either (\e -> error ("Graphics.Plotly.Template: cannot read " <> name <> ": " <> e)) id . eitherDecodeStrict

-- | plotly.py's default template.
plotly :: Template
plotly = decode "plotly" $(embedFile "vendor/plotly.py/templates/plotly.json")
{-# NOINLINE plotly #-}

-- | @plotly_white@: 'plotly' on a white background.
plotlyWhite :: Template
plotlyWhite = decode "plotly_white" $(embedFile "vendor/plotly.py/templates/plotly_white.json")
{-# NOINLINE plotlyWhite #-}

-- | @plotly_dark@: 'plotly' on a dark background.
plotlyDark :: Template
plotlyDark = decode "plotly_dark" $(embedFile "vendor/plotly.py/templates/plotly_dark.json")
{-# NOINLINE plotlyDark #-}

-- | @ggplot2@: in the style of R's ggplot2.
ggplot2 :: Template
ggplot2 = decode "ggplot2" $(embedFile "vendor/plotly.py/templates/ggplot2.json")
{-# NOINLINE ggplot2 #-}

-- | @seaborn@: in the style of Python's seaborn.
seaborn :: Template
seaborn = decode "seaborn" $(embedFile "vendor/plotly.py/templates/seaborn.json")
{-# NOINLINE seaborn #-}

-- | @simple_white@: white, with no grid lines.
simpleWhite :: Template
simpleWhite = decode "simple_white" $(embedFile "vendor/plotly.py/templates/simple_white.json")
{-# NOINLINE simpleWhite #-}

-- | @presentation@: larger text and lines. Combine it with another:
-- @plotlyWhite <> presentation@.
presentation :: Template
presentation = decode "presentation" $(embedFile "vendor/plotly.py/templates/presentation.json")
{-# NOINLINE presentation #-}

-- | @gridon@: grid lines on both axes. Combine it with another.
gridon :: Template
gridon = decode "gridon" $(embedFile "vendor/plotly.py/templates/gridon.json")
{-# NOINLINE gridon #-}

-- | @xgridoff@: no x grid lines. Combine it with another.
xgridoff :: Template
xgridoff = decode "xgridoff" $(embedFile "vendor/plotly.py/templates/xgridoff.json")
{-# NOINLINE xgridoff #-}

-- | @ygridoff@: no y grid lines. Combine it with another.
ygridoff :: Template
ygridoff = decode "ygridoff" $(embedFile "vendor/plotly.py/templates/ygridoff.json")
{-# NOINLINE ygridoff #-}
