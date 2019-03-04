module HaskPapers.Component.Utils
  ( getDailyIndex
  ) where

import Prelude

import Data.Date (Date, canonicalDate, diff)
import Data.Date.Component (Month(..))
import Data.Enum (toEnum)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Days(..))
import HaskPapers.Capability.Now (class Now, nowDate)

getDateDiffMaybe :: Date -> Maybe Days
getDateDiffMaybe date = do
  baseDate <- maybeBaseDate
  pure $ diff date baseDate

getDailyIndex :: forall m. Now m => Int -> m Int
getDailyIndex max = nowDate >>= (pure <<< fromMaybe 0 <<< getIndexMaybe max)

getIndexMaybe :: Int -> Date -> Maybe Int
getIndexMaybe max date = do
  Days number <- getDateDiffMaybe date
  int <- fromNumber number
  pure $ int `mod` max

maybeBaseDate :: Maybe Date
maybeBaseDate = do
  year <- toEnum 2019
  day <- toEnum 1
  pure $ canonicalDate year January day
