{-# LANGUAGE OverloadedStrings #-}

-- |
-- Colour scales and sequences, matching plotly.py's @plotly.colors@. The
-- scales are in four modules, generated from plotly.py's, each best
-- imported qualified:
--
-- @
-- import Graphics.Plotly.Colors (toColorscale)
-- import Graphics.Plotly.Colors.Qualitative qualified as Qualitative
-- import Graphics.Plotly.Colors.Sequential qualified as Sequential
--
-- heat :: Trace Heatmap -> Trace Heatmap
-- heat = #colorscale ?~ toColorscale Sequential.viridis
--
-- palette :: [Color]
-- palette = Qualitative.d3
-- @
--
-- * "Graphics.Plotly.Colors.Qualitative": colour sequences for
--   categories.
-- * "Graphics.Plotly.Colors.Sequential": scales for most continuous
--   data.
-- * "Graphics.Plotly.Colors.Diverging": scales for data with a
--   meaningful midpoint.
-- * "Graphics.Plotly.Colors.Cyclical": scales for data that wraps
--   around.
module Graphics.Plotly.Colors
  ( toColorscale,
    namedColorscale,
    sampleColorscale,
    nColors,
  )
where

import Data.Char (isHexDigit)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Read qualified as TR
import Graphics.Plotly.Colors.Cyclical qualified as Cyclical
import Graphics.Plotly.Colors.Diverging qualified as Diverging
import Graphics.Plotly.Colors.Sequential qualified as Sequential
import Graphics.Plotly.Types (Color (..), Colorscale (..), rgb, rgba)
import Numeric (readHex)

-- | A colour scale through these colours, evenly spaced from 0 to 1, as
-- plotly.py's @make_colorscale@ makes. One colour makes a scale of that
-- colour alone.
toColorscale :: [Color] -> Colorscale
toColorscale = ColorscaleStops . evenStops

-- | Colours spread evenly from 0 to 1.
evenStops :: [Color] -> [(Double, Color)]
evenStops = \case
  [] -> []
  [c] -> [(0, c), (1, c)]
  cs -> zip [fromIntegral i / fromIntegral (length cs - 1) | i <- [0 :: Int ..]] cs

-- | A sequential, diverging or cyclical scale's colours by its plotly.py
-- name, in any case (@"viridis"@, @"RdBu"@), reversed for a name ending in
-- @_r@, as plotly.py's @get_colorscale@ finds them.
namedColorscale :: Text -> Maybe [Color]
namedColorscale name = case T.stripSuffix "_r" lower of
  Just base -> reverse <$> look base
  Nothing -> look lower
  where
    lower = T.toLower name
    look n = Map.lookup n scalesByName

-- | The sequential, diverging and cyclical scales by lowercase name; the
-- first of each name wins.
scalesByName :: Map Text [Color]
scalesByName = Map.fromListWith (\_ old -> old) [(T.toLower n, cs) | (n, cs) <- Sequential.scales <> Diverging.scales <> Cyclical.scales]

-- | The colours of a scale at points from 0 to 1, interpolated between its
-- stops in RGB, as plotly.py's @sample_colorscale@ samples them. A named
-- scale is found with 'namedColorscale'. Points outside 0 to 1 are clamped.
--
-- 'Nothing' if the scale is unknown, has no stops, or has a colour that is
-- not hex (@"#636efa"@, @"#fff"@), @rgb(...)@ or @rgba(...)@.
sampleColorscale :: Colorscale -> [Double] -> Maybe [Color]
sampleColorscale scale points = do
  stops <- case scale of
    ColorscaleNamed n -> stopsOf =<< namedColorscale n
    ColorscaleStops ss -> traverse (traverse parseColor) ss
  case stops of
    [] -> Nothing
    first : _ -> Just (map (sample first stops . max 0 . min 1) points)
  where
    stopsOf = traverse (traverse parseColor) . evenStops
    sample first stops p = case [(lo, hi) | (lo, hi) <- zip stops (drop 1 stops), fst lo <= p, p <= fst hi] of
      ((l, lc), (h, hc)) : _
        | h > l -> render (mix lc hc ((p - l) / (h - l)))
        | otherwise -> render lc
      [] -> render (snd (if p <= fst first then first else last stops))

-- | @n@ colours from one to another, evenly spaced in RGB, both ends
-- included, as plotly.py's @n_colors@ makes them. 'Nothing' if either
-- colour is not hex, @rgb(...)@ or @rgba(...)@.
nColors :: Color -> Color -> Int -> Maybe [Color]
nColors from to n = do
  a <- parseColor from
  b <- parseColor to
  pure $ case n of
    _ | n <= 0 -> []
    1 -> [render a]
    _ -> [render (mix a b (fromIntegral i / fromIntegral (n - 1))) | i <- [0 .. n - 1]]

-- | Red, green and blue from 0 to 255, and opacity from 0 to 1.
data RGBA = RGBA Double Double Double Double

mix :: RGBA -> RGBA -> Double -> RGBA
mix (RGBA r g b a) (RGBA r' g' b' a') t = RGBA (r + (r' - r) * t) (g + (g' - g) * t) (b + (b' - b) * t) (a + (a' - a) * t)

render :: RGBA -> Color
render (RGBA r g b a)
  | a >= 1 = rgb (byte r) (byte g) (byte b)
  | otherwise = rgba (byte r) (byte g) (byte b) a
  where
    byte = round . max 0 . min 255

parseColor :: Color -> Maybe RGBA
parseColor = \case
  ColorNumber _ -> Nothing
  ColorString s -> parse (T.toLower (T.strip s))
  where
    parse s
      | Just hex <- T.stripPrefix "#" s,
        T.all isHexDigit hex = case T.length hex of
          3 -> fromHex (T.concatMap (\c -> T.pack [c, c]) hex)
          6 -> fromHex hex
          _ -> Nothing
      | Just body <- T.stripPrefix "rgba(" s >>= T.stripSuffix ")" = case components body of
          Just [r, g, b, a] -> Just (RGBA r g b a)
          _ -> Nothing
      | Just body <- T.stripPrefix "rgb(" s >>= T.stripSuffix ")" = case components body of
          Just [r, g, b] -> Just (RGBA r g b 1)
          _ -> Nothing
      | otherwise = Nothing
    fromHex hex = case [n | [(n, "")] <- map (readHex . T.unpack) (T.chunksOf 2 hex)] of
      [r, g, b] -> Just (RGBA (fromInteger r) (fromInteger g) (fromInteger b) 1)
      _ -> Nothing
    components body = traverse number (T.splitOn "," body)
    number t = case TR.double (T.strip t) of
      Right (d, "") -> Just d
      _ -> Nothing
