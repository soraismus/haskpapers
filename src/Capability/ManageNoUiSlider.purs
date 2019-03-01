module HaskPapers.Capability.ManageNoUiSlider
  ( class ManageNoUiSlider
  , createNoUiSlider
  , updateNoUiSlider
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Halogen (HalogenM)
import HaskPapers.Foreign.NoUiSlider (CreateNoUiSlider, UpdateNoUiSlider)

class Monad m <= ManageNoUiSlider m where
  createNoUiSlider :: CreateNoUiSlider -> m Unit
  updateNoUiSlider :: UpdateNoUiSlider -> m Unit

instance manageNoUiSliderHalogenM
  :: ManageNoUiSlider m
  => ManageNoUiSlider (HalogenM s f g p o m) where
    createNoUiSlider = lift <<< createNoUiSlider
    updateNoUiSlider = lift <<< updateNoUiSlider
