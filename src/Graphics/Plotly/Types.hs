{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- The value types of plotly.js attributes, one for each kind of value in
-- plotly.js's schema. Enumerations are in "Graphics.Plotly.Enums".
module Graphics.Plotly.Types
  ( -- * Colours
    Color (..),
    rgb,
    rgba,
    Colorscale (..),

    -- * Flags
    Flags (..),

    -- * Values that may vary per point
    ArrayOk (..),

    -- * Data arrays
    DataArray (..),
    dataArray,
    fromDataArray,

    -- * Subplots
    SubplotId (..),

    -- * Angles
    Angle (..),

    -- * For generated code
    parseEnum,
    enumMismatch,
  )
where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Foldable qualified as F
import Data.List (find)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word8)
import GHC.IsList (IsList (..))
import Numeric (showFFloat)

-- | A colour: a CSS colour string (@"red"@, @"#d3d3d3"@,
-- @"rgba(255, 0, 0, 0.5)"@), or a number, which a trace with a colour scale
-- maps to a colour.
data Color
  = ColorString Text
  | ColorNumber Double
  deriving stock (Eq, Ord, Show)

instance IsString Color where
  fromString = ColorString . T.pack

instance ToJSON Color where
  toJSON (ColorString s) = String s
  toJSON (ColorNumber n) = toJSON n

instance FromJSON Color where
  parseJSON (String s) = pure (ColorString s)
  parseJSON v@(Number _) = ColorNumber <$> parseJSON v
  parseJSON v = fail ("Color: unexpected " <> show v)

-- | An opaque colour from red, green and blue components.
rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = ColorString ("rgb(" <> commas [showT r, showT g, showT b] <> ")")

-- | A colour from red, green and blue components and an opacity from 0 to 1.
rgba :: Word8 -> Word8 -> Word8 -> Double -> Color
rgba r g b a = ColorString ("rgba(" <> commas [showT r, showT g, showT b, alpha] <> ")")
  where
    -- Without an exponent, which CSS parsers such as plotly.js's reject:
    -- @show 0.05@ is @5.0e-2@.
    alpha = T.pack (showFFloat Nothing (max 0 (min 1 a)) "")

commas :: [Text] -> Text
commas = T.intercalate ", "

showT :: (Show a) => a -> Text
showT = T.pack . show

-- | A colour scale: a plotly.js name (@"Viridis"@, @"RdBu"@, …), or stops
-- from 0 to 1, each with a colour.
data Colorscale
  = ColorscaleNamed Text
  | ColorscaleStops [(Double, Color)]
  deriving stock (Eq, Show)

instance IsString Colorscale where
  fromString = ColorscaleNamed . T.pack

instance ToJSON Colorscale where
  toJSON (ColorscaleNamed n) = String n
  toJSON (ColorscaleStops stops) = toJSON stops

instance FromJSON Colorscale where
  parseJSON (String n) = pure (ColorscaleNamed n)
  parseJSON v = ColorscaleStops <$> parseJSON v

-- | A combination of flags, such as a scatter trace's
-- @Flags [ModeLines, ModeMarkers]@, sent as @"lines+markers"@. A flag that
-- stands alone (such as @ModeNone@) goes in a list by itself.
newtype Flags a = Flags [a]
  deriving stock (Eq, Ord, Show)

instance IsList (Flags a) where
  type Item (Flags a) = a
  fromList = Flags
  toList (Flags fs) = fs

instance (ToJSON a) => ToJSON (Flags a) where
  toJSON (Flags [f]) = toJSON f
  toJSON (Flags fs) = String (T.intercalate "+" [s | String s <- map toJSON fs])

instance (FromJSON a) => FromJSON (Flags a) where
  parseJSON (String s) = Flags <$> traverse (parseJSON . String) (T.splitOn "+" s)
  parseJSON v = Flags . pure <$> parseJSON v

-- | A value for every point, or one for them all.
data ArrayOk a
  = One a
  | Many [a]
  deriving stock (Eq, Show)

-- | v'One'.
instance (IsString a) => IsString (ArrayOk a) where
  fromString = One . fromString

-- | v'Many'.
instance IsList (ArrayOk a) where
  type Item (ArrayOk a) = a
  fromList = Many
  toList (One a) = [a]
  toList (Many as) = as

instance (ToJSON a) => ToJSON (ArrayOk a) where
  toJSON (One a) = toJSON a
  toJSON (Many as) = toJSON as

instance (FromJSON a) => FromJSON (ArrayOk a) where
  parseJSON v@(Array _) = Many <$> parseJSON v
  parseJSON v = One <$> parseJSON v

-- | An array of data: numbers, strings, dates, or nested arrays for 2D
-- data. Build one with 'dataArray'.
newtype DataArray = DataArray (V.Vector Value)
  deriving stock (Eq, Show)

-- | Any 'ToJSON' values.
dataArray :: (Foldable f, ToJSON a) => f a -> DataArray
dataArray = DataArray . V.fromList . map toJSON . F.toList

-- | The values, if they all decode.
fromDataArray :: (FromJSON a) => DataArray -> Maybe [a]
fromDataArray (DataArray vs) = traverse (resultToMaybe . fromJSON) (V.toList vs)
  where
    resultToMaybe (Success a) = Just a
    resultToMaybe (Error _) = Nothing

instance IsList DataArray where
  type Item DataArray = Value
  fromList = DataArray . V.fromList
  toList (DataArray vs) = V.toList vs

instance ToJSON DataArray where
  toJSON (DataArray vs) = Array vs

instance FromJSON DataArray where
  parseJSON = withArray "DataArray" (pure . DataArray)

-- | A subplot's id: @"x"@, @"x2"@, @"geo3"@, ….
newtype SubplotId = SubplotId Text
  deriving stock (Eq, Ord, Show)
  deriving newtype (IsString, ToJSON, FromJSON)

-- | An angle in degrees, from -180 to 180.
newtype Angle = Angle Double
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional, ToJSON, FromJSON)

-- | Decode an enumeration: the constructor that encodes as the value, or
-- else the fallback.
parseEnum :: (ToJSON a) => [a] -> (Value -> Parser a) -> Value -> Parser a
parseEnum ctors fallback v = maybe (fallback v) pure (find ((== v) . toJSON) ctors)

-- | Fail to decode the named enumeration.
enumMismatch :: String -> Value -> Parser a
enumMismatch name v = fail (name <> ": unexpected " <> show v)
