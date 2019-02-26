module HaskPapers.Data.Archive
  ( Archive(..)
  , decodeArchive
  ) where

import Prelude

import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Either (Either)
import Data.Map (Map)
import Data.Set (Set)
import Foreign.Object as FO
import HaskPapers.Data.Author (Author)
import HaskPapers.Data.Id (Id)
import HaskPapers.Data.Link (Link)
import HaskPapers.Data.Paper (Paper, decodePaper)
import HaskPapers.Data.Title (Title)
import HaskPapers.Data.Utils (tryGet)
import HaskPapers.Data.Year (Year)

type Archive =
  { titles :: Map Id Title
  , authors :: Map Id Author
  , links :: Map Id Link
  , authorsIndex :: Map Id (Set Id)
  , yearMin :: Year
  , yearMax :: Year
  , papers :: Array Paper
  }

authorsKey :: String
authorsKey = "b"

authorsIndexKey :: String
authorsIndexKey = "e"

linksKey :: String
linksKey = "c"

papersKey :: String
papersKey = "d"

titlesKey :: String
titlesKey = "a"

yearMaxKey :: String
yearMaxKey = "g"

yearMinKey :: String
yearMinKey = "f"

decodeArchive :: Json -> Either String Archive
decodeArchive json = do
  obj          <- decodeJson json
  titles       <- obj .: titlesKey
  authors      <- obj .: authorsKey
  links        <- obj .: linksKey
  authorsIndex <- obj .: authorsIndexKey
  yearMin      <- obj .: yearMinKey
  yearMax      <- obj .: yearMaxKey
  papers       <- tryGetPapers papersKey titles authors links obj
  pure
    { titles
    , authors
    , links
    , authorsIndex
    , yearMin
    , yearMax
    , papers
    }

tryGetPapers
  :: String
  -> Map Id Title
  -> Map Id Author
  -> Map Id Link
  -> FO.Object Json
  -> Either String (Array Paper)
tryGetPapers key titles authors links =
  tryGet key (decodePaper titles authors links)
