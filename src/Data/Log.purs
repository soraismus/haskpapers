module HaskPapers.Data.Log
  ( Log
  , LogReason(..)
  , message
  , reason
  , mkLog
  , timestamp
  ) where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Formatter.DateTime (formatDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import HaskPapers.Capability.Now (class Now, nowDateTime)

data LogReason = Debug | Info | Warn | Error

derive instance eqLogReason      :: Eq      LogReason
derive instance genericLogReason :: Generic LogReason _
derive instance ordLogReason     :: Ord     LogReason

instance showLogReason :: Show LogReason where
  show = genericShow

newtype Log = Log
  { reason    :: LogReason
  , timestamp :: DateTime
  , message   :: String
  }

derive instance eqLog      :: Eq      Log
derive instance genericLog :: Generic Log _

message :: Log -> String
message (Log { message: m }) = m

mkLog :: forall m. Now m => LogReason -> String -> m Log
mkLog logReason inputMessage = do
  now <- nowDateTime

  let
    -- Will format "2018-10-25 11:25:29 AM"
    formatTimestamp =
      either (const "(Failed to assign time)") identity
      <<< formatDateTime "YYYY-DD-MM hh:mm:ss a"

    -- Will produce a header like
    -- "{DEBUG: 2018-10-25 11:25:29 AM]\nMessage contents..."
    headerWith start =
         "["
      <> start
      <> ": "
      <> formatTimestamp now
      <> "]\n"
      <> inputMessage

    formattedLog = case logReason of
      Debug -> headerWith "DEBUG"
      Info  -> headerWith "INFO"
      Warn  -> headerWith "WARNING"
      Error -> headerWith "ERROR"

  pure $ Log { reason: logReason, timestamp: now, message: formattedLog }

reason :: Log -> LogReason
reason (Log { reason: r }) = r

timestamp :: Log -> DateTime
timestamp (Log { timestamp: t }) = t
