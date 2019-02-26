module HaskPapers.Data.Paper
  ( Paper
  , decodePaper
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, decodeJson, (.:), (.:?))
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Traversable (sequence)
import Foreign.Object as FO
import HaskPapers.Data.Author (Author)
import HaskPapers.Data.Id (Id)
import HaskPapers.Data.Link (Link)
import HaskPapers.Data.Title (Title)
import HaskPapers.Data.Year (Year)

type Paper =
  { titleId :: Id
  , title :: Title
  , authors :: Array Author
  , yearMaybe :: Maybe Year
  , references :: Array Title
  , citations :: Array Title
  , links :: Array Link
  , loc :: { file :: Int, line :: Int }
  }

authorsKey :: String
authorsKey = "b"

citationsKey :: String
citationsKey = "h"

fileKey :: String
fileKey = "f"

lineKey :: String
lineKey = "g"

linksKey :: String
linksKey = "e"

referencesKey :: String
referencesKey = "d"

titleIdKey :: String
titleIdKey = "a"

yearMaybeKey :: String
yearMaybeKey = "c"

decodePaper
  :: Map Id Title
  -> Map Id Author
  -> Map Id Link
  -> Json
  -> Either String Paper
decodePaper titleMap authorMap linkMap json = do
  obj        <- decodeJson json

  titleId    <- obj .:  titleIdKey
  file       <- obj .:  fileKey
  line       <- obj .:  lineKey
  yearMaybe  <- obj .:? yearMaybeKey

  authors    <- tryGetArray "author"          authorsKey    authorMap obj
  references <- tryGetArray "reference title" referencesKey titleMap  obj
  citations  <- tryGetArray "citation title"  citationsKey  titleMap  obj
  links      <- tryGetArray "link"            linksKey      linkMap   obj

  title      <- tryLookup titleId titleMap

  pure
    { titleId
    , title
    , authors
    , yearMaybe
    , references
    , citations
    , links
    , loc: { file, line }
    }

  where
  tryLookup id map =
    note ("No title corresponds to id " <> show id) $ Map.lookup id map

tryGetArray
  :: forall a
   . String
  -> String
  -> Map Id a
  -> FO.Object Json
  -> Either String (Array a)
tryGetArray label key _map =
  _tryGetArray key (\id -> note
    ("No " <> label <> " corresponds to id " <> show id <> ".")
    (Map.lookup id _map))


_tryGetArray
  :: forall a b
   . DecodeJson a
  => String
  -> (a -> Either String b)
  -> FO.Object Json
  -> Either String (Array b)
_tryGetArray key fn =
  join
    <<< map sequence
    <<< (map <<< map) fn
    <<< getArrayField key

getArrayField
  :: forall a
   . DecodeJson a
  => String
  -> FO.Object Json
  -> Either String (Array a)
getArrayField str obj =
  maybe
    (Right [])
    (elaborateFailure str <<< decodeJson)
    (FO.lookup str obj)

elaborateFailure :: forall a. String -> Either String a -> Either String a
elaborateFailure str result =
  lmap msg result
  where
    msg innerMsg = "Failed to decode key '" <> str <> "': " <> innerMsg
