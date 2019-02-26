module HaskPapers.Data.Author
  ( Author
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

newtype Author = Author String

derive instance eqAuthor      :: Eq      Author
derive instance genericAuthor :: Generic Author _

derive newtype instance decodeJsonAuthor :: DecodeJson Author
derive newtype instance encodeJsonAuthor :: EncodeJson Author

instance showAuthor :: Show Author where
  show = genericShow

instance toHtmlStringAuthor :: ToHtmlString Author where
  toHtmlString (Author str) = str

parse :: String -> Maybe Author
parse "" = Nothing
parse str = Just (Author str)

toString :: Author -> String
toString (Author str) = str
