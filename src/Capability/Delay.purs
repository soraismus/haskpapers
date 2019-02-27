module HaskPapers.Capability.Delay
  ( class Delay
  , DelayId
  , delay
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Halogen (HalogenM)

newtype DelayId = DelayId Int

class Monad m <= Delay m where
  delay :: Int -> Effect Unit -> m DelayId

instance delayHalogenM :: Delay m => Delay (HalogenM s f g p o m) where
  delay interval effect = lift (delay interval effect)
