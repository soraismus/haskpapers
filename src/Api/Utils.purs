module HaskPapers.Api.Utils
  ( decode
  , decodeAt
  , mkRequest
  ) where

import Prelude

import Affjax (request)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:))
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import HaskPapers.Api.Request (BaseURL, RequestOptions, formJsonRequest)
import HaskPapers.Capability.LogMessages (class LogMessages, logError)
import HaskPapers.Capability.Now (class Now)

decode
  :: forall m a
   . LogMessages m
  => Now m
  => (Json -> Either String a)
  -> Maybe Json
  -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing
decode decoder (Just json) = case decoder json of
  Left err -> logError err *> pure Nothing
  Right response -> pure (Just response)

decodeAt :: forall a. DecodeJson a => String -> Json -> Either String a
decodeAt key = decodeJson <=< (_ .: key) <=< decodeJson

mkRequest
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Maybe Json)
mkRequest opts = do
  { baseUrl } <- ask
  response <- liftAff $ request $ formJsonRequest baseUrl opts
  pure $ hush response.body
