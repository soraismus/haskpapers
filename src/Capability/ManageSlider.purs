module HaskPapers.Capability.ManageSlider
  ( class ManageSlider
  , createSlider
  --, onSliderUpdate
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import Halogen (HalogenM)
import HaskPapers.Foreign.Slider (Slider, SliderUpdate)

class Monad m <= ManageSlider m where
  createSlider :: Slider -> m Unit
  --onSliderUpdate :: (SliderUpdate -> Effect Unit) -> Effect (Effect Unit)

instance manageSliderHalogenM
  :: ManageSlider m
  => ManageSlider (HalogenM s f g p o m) where
    createSlider = lift <<< createSlider
    -- onSliderUpdate = lift <<< onSliderUpdate
