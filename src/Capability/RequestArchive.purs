module HaskPapers.Capability.RequestArchive
  ( class RequestArchive
  , requestArchive
  ) where

import Prelude

import Data.Maybe (Maybe)
import Control.Monad.Trans.Class (lift)
import HaskPapers.Data.Archive (Archive)
import HaskPapers.Data.WrappedDate (WrappedDate)
import Halogen (HalogenM)

class Monad m <= RequestArchive m where
  requestArchive :: m (Maybe { archive :: Archive, date :: WrappedDate })

instance requestArchiveHalogenM
  :: RequestArchive m
  => RequestArchive (HalogenM s f g p o m) where
    requestArchive = lift requestArchive
