module HaskPapers.Data.Title
  ( Title
  , parse
  , toString
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import HaskPapers.Data.ToHtmlString (class ToHtmlString, toHtmlString)

newtype Title = Title String

derive instance eqTitle      :: Eq      Title
derive instance genericTitle :: Generic Title _

derive newtype instance encodeJsonTitle :: EncodeJson Title
derive newtype instance decodeJsonTitle :: DecodeJson Title

instance showTitle :: Show Title where
  show = genericShow

instance toHtmlStringTitle :: ToHtmlString Title where
  toHtmlString (Title str) = str

parse :: String -> Maybe Title
parse "" = Nothing
parse str = Just (Title str)

toString :: Title -> String
toString (Title str) = str
