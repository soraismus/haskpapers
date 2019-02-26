module HaskPapers.Data.ToHtmlString
  (class ToHtmlString
  , toHtmlString
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)

class ToHtmlString a where
  toHtmlString :: a -> String
