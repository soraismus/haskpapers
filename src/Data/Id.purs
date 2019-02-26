module HaskPapers.Data.Id
  ( Id
  , toIdMaybe
  , toInt
  , toString
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))

newtype Id = Id Int

derive instance eqId      :: Eq      Id
derive instance genericId :: Generic Id _

derive newtype instance encodeJsonId :: EncodeJson Id
derive newtype instance decodeJsonId :: DecodeJson Id

instance ordId :: Ord Id where
  compare (Id x) (Id y) = compare x y

instance showId :: Show Id where
  show = genericShow

toIdMaybe :: Int -> Maybe Id
toIdMaybe int = Just (Id int)

toInt :: Id -> Int
toInt (Id int) = int

toString :: Id -> String
toString (Id int) = show int
