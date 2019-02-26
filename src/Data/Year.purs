module HaskPapers.Data.Year
  ( Year
  , toInt
  , toYearMaybe
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

newtype Year = Year Int

derive instance eqYear      :: Eq      Year
derive instance genericYear :: Generic Year _

derive newtype instance decodeJsonYear :: DecodeJson Year
derive newtype instance encodeJsonYear :: EncodeJson Year

instance showYear :: Show Year where
  show = genericShow

toInt :: Year -> Int
toInt (Year int) = int

toYearMaybe :: Int -> Maybe Year
toYearMaybe int = Just (Year int)
