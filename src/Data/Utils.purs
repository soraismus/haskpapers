module HaskPapers.Data.Utils
  ( tryGet
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, getField)
import Data.Either (Either)
import Data.Traversable (sequence)
import Foreign.Object as FO

tryGet
  :: forall a b
   . DecodeJson a
  => String
  -> (a -> Either String b)
  -> FO.Object Json
  -> Either String (Array b)
tryGet key fn =
  join
    <<< map sequence
    <<< (map <<< map) fn
    <<< flip getField key
