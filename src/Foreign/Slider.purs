module HaskPapers.Foreign.Slider 
  ( Slider
  , SliderYears
  , onSliderUpdate
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)

type Slider =
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

type SliderYears = Array Int

foreign import onSliderUpdate
  :: Slider
  -> (SliderYears -> Effect Unit)
  -> Effect (Effect Unit)
