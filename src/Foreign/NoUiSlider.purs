module HaskPapers.Foreign.NoUiSlider 
  ( CreateNoUiSlider
  , UpdateNoUiSlider
  , createNoUiSliderEffect
  , updateNoUiSliderEffect
  ) where

import Data.Maybe (Maybe)
import Effect (Effect)

type CreateNoUiSlider =
  { id          :: String        -- The element id to create a slider in.
  , start       :: Array Int     -- The starting handles' starting positions.
  , margin      :: Maybe Int     -- How far apart *must* handles be?
  , limit       :: Maybe Int     -- How far apart *may* handles be?
  , connect     :: Maybe Boolean -- Display a colored bar between the handles?
  , direction   :: Maybe String  -- Not sure what this is.
  , orientation :: Maybe String
  , behavior    :: Maybe String
  , step        :: Maybe Int
  , range       :: Maybe { min :: Int, max :: Int }
  }

foreign import createNoUiSliderEffect
  :: forall a
   . CreateNoUiSlider
  -> Effect a

type UpdateNoUiSlider = Array Int

foreign import updateNoUiSliderEffect
  :: forall a
   . UpdateNoUiSlider
  -> Effect a
