module HaskPapers.Capability.LogMessages
  ( class LogMessages
  , debugHush
  , log
  , logDebug
  , logError
  , logHush
  , logInfo
  , logMessage
  , logWarn
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM)
import HaskPapers.Capability.Now (class Now)
import HaskPapers.Data.Log (Log, LogReason(..), mkLog)

class Monad m <= LogMessages m where
  logMessage :: Log -> m Unit

instance logMessagesHalogenM
  :: LogMessages m
  => LogMessages (HalogenM s f g p o m) where
    logMessage = lift <<< logMessage

debugHush
  :: forall m a. LogMessages m
  => Now m
  => m (Either String a)
  -> m (Maybe a)
debugHush = logHush Debug

log :: forall m. LogMessages m => Now m => LogReason -> String -> m Unit
log reason = logMessage <=< mkLog reason

logDebug :: forall m. LogMessages m => Now m => String -> m Unit
logDebug = log Debug

logError :: forall m. LogMessages m => Now m => String -> m Unit
logError = log Error

logHush
  :: forall m a. LogMessages m
  => Now m
  => LogReason
  -> m (Either String a)
  -> m (Maybe a)
logHush reason action =
  action >>= case _ of
    Left error  -> case reason of
              Debug -> logDebug error *> pure Nothing
              Info  -> logInfo error  *> pure Nothing
              Warn  -> logWarn error  *> pure Nothing
              Error -> logError error *> pure Nothing
    Right value -> pure $ Just value

logInfo :: forall m. LogMessages m =>  Now m =>String -> m Unit
logInfo = log Info

logWarn :: forall m. LogMessages m =>  Now m =>String -> m Unit
logWarn = log Warn
