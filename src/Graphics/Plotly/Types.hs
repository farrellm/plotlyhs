{-# LANGUAGE AllowAmbiguousTypes #-}
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
    NonEmpty (..),

    -- * Values that may vary per point
    ArrayOk (..),

    -- * Data arrays
    DataArray (..),
    dataArray,
    fromDataArray,

    -- * Subplots
    SubplotId (..),
    subplotIdText,
    subplotNumber,

    -- ** Cartesian axes
    Dim (..),
    KnownDim (..),
    AxisId (..),
    axisIdText,
    AxisRef (..),

    -- * Angles
    Angle (..),

    -- * Templates
    Template (..),

    -- * For generated code
    parseEnum,
    enumMismatch,
  )
where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Char (isDigit)
import Data.Foldable qualified as F
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Word (Word8)
import GHC.IsList (IsList (..))
import Numeric (showFFloat)
import Text.Read (readMaybe)

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

-- | A flag list: flags combined with @+@, or one value that stands alone.
--
-- A scatter trace's @mode@ is either @Flags (ModeLines :| [ModeMarkers])@
-- (sent as @"lines+markers"@) or @Extra ModeNone@ (sent as @"none"@). @e@
-- is the attribute's own type of extras: 'Bool' where they are @true@ and
-- @false@, and 'Data.Void.Void' where it has none.
data Flags f e
  = Flags (NonEmpty f)
  | Extra e
  deriving stock (Eq, Ord, Show)

instance (ToJSON f, ToJSON e) => ToJSON (Flags f e) where
  toJSON (Flags (f :| [])) = toJSON f
  toJSON (Flags fs) = String (T.intercalate "+" [s | String s <- map toJSON (F.toList fs)])
  toJSON (Extra e) = toJSON e

instance (FromJSON f, FromJSON e) => FromJSON (Flags f e) where
  parseJSON v = (Extra <$> parseJSON v) <|> withText "Flags" flags v
    where
      flags s = case T.splitOn "+" s of
        f : fs -> Flags <$> traverse (parseJSON . String) (f :| fs)
        [] -> fail "no flags"

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

-- | The id of a numbered subplot or axis: @subplotIdText "x" 1@ is @"x"@,
-- and @subplotIdText "scene" 2@ is @"scene2"@.
subplotIdText :: Text -> Int -> Text
subplotIdText base n
  | n <= 1 = base
  | otherwise = base <> T.pack (show n)

-- | The number of a subplot id or layout key with this prefix, as plotly.js
-- reads them: @subplotNumber "x" "x"@ is @Just 1@ and
-- @subplotNumber "xaxis" "xaxis2"@ is @Just 2@. @"x1"@ and @"x02"@ are not
-- ids.
subplotNumber :: Text -> Text -> Maybe Int
subplotNumber base s = case T.stripPrefix base s of
  Just "" -> Just 1
  Just n
    | Just (c, _) <- T.uncons n,
      c /= '0',
      T.all isDigit n,
      Just i <- readMaybe (T.unpack n),
      i >= 2 ->
        Just i
  _ -> Nothing

-- | The two dimensions of cartesian axes. As a type, it says which an
-- 'AxisId' or 'AxisRef' is of.
data Dim = X | Y
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | A dimension known at compile time.
class KnownDim (d :: Dim) where
  dimVal :: Dim

instance KnownDim 'X where dimVal = X

instance KnownDim 'Y where dimVal = Y

-- | A cartesian axis of dimension @d@, by its number from 1:
-- @AxisId 1 :: AxisId 'X@ is the x axis, @"x"@ (@layout.xaxis@), and
-- @AxisId 2 :: AxisId 'Y@ is @"y2"@ (@layout.yaxis2@).
newtype AxisId (d :: Dim) = AxisId Int
  deriving stock (Eq, Ord, Show)

type role AxisId nominal

-- | The axis's id: @"x"@, @"y2"@, ….
axisIdText :: forall d. (KnownDim d) => AxisId d -> Text
axisIdText (AxisId n) = subplotIdText (dimName (dimVal @d)) n

dimName :: Dim -> Text
dimName = \case
  X -> "x"
  Y -> "y"

instance (KnownDim d) => ToJSON (AxisId d) where
  toJSON = String . axisIdText

instance (KnownDim d) => FromJSON (AxisId d) where
  parseJSON = withText "AxisId" $ \s -> case subplotNumber (dimName (dimVal @d)) s of
    Just n -> pure (AxisId n)
    Nothing -> fail ("AxisId: not an " <> T.unpack (dimName (dimVal @d)) <> " axis: " <> show s)

-- | What a shape, annotation, image or selection measures its position
-- along dimension @d@ against: its @xref@ or @yref@.
data AxisRef (d :: Dim)
  = -- | The plotting area, from 0 at the left or bottom to 1: @"paper"@.
    Paper
  | -- | An axis's data coordinates: @"x2"@.
    Axis (AxisId d)
  | -- | An axis's domain, from 0 to 1, like 'Paper' but along the axis:
    -- @"x2 domain"@.
    AxisDomain (AxisId d)
  deriving stock (Eq, Ord, Show)

type role AxisRef nominal

instance (KnownDim d) => ToJSON (AxisRef d) where
  toJSON = \case
    Paper -> String "paper"
    Axis a -> toJSON a
    AxisDomain a -> String (axisIdText a <> " domain")

instance (KnownDim d) => FromJSON (AxisRef d) where
  parseJSON = withText "AxisRef" $ \case
    "paper" -> pure Paper
    s
      | Just a <- T.stripSuffix " domain" s -> AxisDomain <$> parseJSON (String a)
      | otherwise -> Axis <$> parseJSON (String s)

-- | An angle in degrees, from -180 to 180.
newtype Angle = Angle Double
  deriving stock (Eq, Ord, Show)
  deriving newtype (Num, Fractional, ToJSON, FromJSON)

-- | A template: default attributes for a figure's layout and its traces,
-- as @{layout: {...}, data: {scatter: [{...}, ...], ...}}@. Set one with
-- @#layout % #template@. "Graphics.Plotly.Template" has plotly.py's.
--
-- @a <> b@ merges two templates as plotly.py's @merge_templates@ does: @b@'s
-- attributes win. Each trace type's list of trace templates is repeated on
-- both sides to the same length, the least common multiple of the two, and
-- merged item by item; an empty list leaves the other side's. Elsewhere,
-- arrays of objects (@annotations@, @shapes@, …) are merged item by item,
-- keeping the longer one's extra items, as plotly.py's @update@ merges
-- them.
newtype Template = Template Object
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON)

instance Semigroup Template where
  Template a <> Template b = Template (KM.unionWithKey merge a b)
    where
      merge "data" (Object x) (Object y) = Object (KM.unionWith mergeTraces x y)
      merge _ x y = mergeTemplateValue x y
      mergeTraces (Array xs) (Array ys)
        | not (V.null xs) && not (V.null ys) =
            let n = lcm (V.length xs) (V.length ys)
                cycled v = V.generate n (\i -> v V.! (i `mod` V.length v))
             in Array (V.zipWith mergeTemplateValue (cycled xs) (cycled ys))
      mergeTraces x y = mergeTemplateValue x y

-- | Merge two values of a template as plotly.py's @update@ does: objects
-- deeply, and arrays of objects item by item, keeping the longer one's
-- extra items (an empty array leaves the other); any other value of the
-- second wins.
mergeTemplateValue :: Value -> Value -> Value
mergeTemplateValue (Object a) (Object b) = Object (KM.unionWith mergeTemplateValue a b)
mergeTemplateValue (Array xs) (Array ys)
  | not (V.null xs) && all isObject xs && all isObject ys =
      Array (V.zipWith mergeTemplateValue xs ys <> V.drop (V.length ys) xs <> V.drop (V.length xs) ys)
  where
    isObject = \case
      Object _ -> True
      _ -> False
mergeTemplateValue _ b = b

instance Monoid Template where
  mempty = Template KM.empty

-- | Decode an enumeration: the constructor that encodes as the value, or
-- else the fallback.
parseEnum :: (ToJSON a) => [a] -> (Value -> Parser a) -> Value -> Parser a
parseEnum ctors fallback v = maybe (fallback v) pure (find ((== v) . toJSON) ctors)

-- | Fail to decode the named enumeration.
enumMismatch :: String -> Value -> Parser a
enumMismatch name v = fail (name <> ": unexpected " <> show v)
