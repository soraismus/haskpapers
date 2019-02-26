module HaskPapers.Capability.Now
  ( class Now
  , now
  , nowDate
  , nowDateTime
  , nowTime
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.DateTime (Date, DateTime, Time)
import Data.DateTime.Instant (Instant)
import Halogen (HalogenM)

class Monad m <= Now m where
  now         :: m Instant
  nowDate     :: m Date
  nowDateTime :: m DateTime
  nowTime     :: m Time

instance nowHalogenM :: Now m => Now (HalogenM s f g p o m) where
  now         = lift now
  nowDate     = lift nowDate
  nowDateTime = lift nowDateTime
  nowTime     = lift nowTime
