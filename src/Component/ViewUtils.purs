module HaskPapers.Component.ViewUtils
  ( class_
  , padLeft3
  ) where

import Prelude

import Data.String (length)
import Halogen.HTML (IProp, ClassName(..))
import Halogen.HTML.Properties as HP

class_ :: forall r i. String -> IProp ( class :: String | r ) i
class_ = HP.class_ <<< ClassName

padLeft3 :: String -> String
padLeft3 str
  | length str == 0 = "000"
  | length str == 1 = "00" <> str
  | length str == 2 = "0" <> str
  | otherwise              = str
