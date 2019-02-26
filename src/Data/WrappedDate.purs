module HaskPapers.Data.WrappedDate
  ( WrappedDate(..)
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Date (Date)
import Data.DateTime (DateTime(..))
import Data.Either (Either, note)
import Data.Newtype (class Newtype)
import Data.RFC3339String (RFC3339String(..), toDateTime)
import HaskPapers.Data.ToHtmlString (class ToHtmlString, toHtmlString)

newtype WrappedDate = WrappedDate Date

derive instance eqWrappedDate      :: Eq      WrappedDate
derive instance newtypeWrappedDate :: Newtype WrappedDate _

instance decodeJsonWrappedDate :: DecodeJson WrappedDate where
  decodeJson = fromString <=< decodeJson

instance showWrappedDate :: Show WrappedDate where
  show (WrappedDate date) = show date

instance toHtmlStringWrappedDate :: ToHtmlString WrappedDate where
  toHtmlString (WrappedDate date) = show date

fromString :: String -> Either String WrappedDate
fromString = 
  map (\(DateTime date _) -> WrappedDate date)
    <<< note "Could not parse RFC339 string" 
    <<< toDateTime
    <<< RFC3339String
