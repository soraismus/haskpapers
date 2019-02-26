module HaskPapers.Component.Root
  ( Query
  , component
  ) where

import Prelude

import Data.Date (Date)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Map (Map)
import Data.Maybe (Maybe(..), maybe)
import Data.Set (Set)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import HaskPapers.Capability.LogMessages (class LogMessages)
import HaskPapers.Capability.Now (class Now)
import HaskPapers.Capability.RequestArchive
  ( class RequestArchive
  , requestArchive
  )
import HaskPapers.Component.ComponentA as CA
import HaskPapers.Component.ComponentB as CB
import HaskPapers.Component.ComponentC as CC
import HaskPapers.Data.Archive (Archive)
import HaskPapers.Data.Author (Author)
import HaskPapers.Data.Id (Id)
import HaskPapers.Data.Paper (Paper)
import HaskPapers.Data.Year (Year, toInt)
import HaskPapers.Data.WrappedDate (WrappedDate(..))

data RenderAmount = RenderAll | RenderSome

type StateRec =
  { now :: Date
  , papers :: Array Paper
  , authors :: Map Id Author
  , yearMin :: Year
  , yearMax :: Year
  , authorFacets :: Array { name :: String, titleIds :: Set Id }
  , visibleIds :: Set Id
  , renderAmount :: RenderAmount
  , filters ::
      { title :: String
      , titleIds :: Set Id
      , author :: String
      , authorIds :: Set Id
      , yearMin :: Int
      , yearMax :: Int
      }
  }

type HttpResponse body =
  { url :: String
  , status :: { code :: Int, message :: String }
  , headers :: Map String String
  , body :: body
  }

data HttpError
  = BadUrl String
  | Timeout
  | NetworkError
  | BadStatus (HttpResponse String)
  | BadPayload String (HttpResponse String)

--type State = RemoteData HttpError StateRec
data State
  = NotAsked
  | Loading
  | Loaded StateRec
  | LoadError

data Query a
  = RequestArchive a
  | DisplayArchive Archive Date a
  | RenderMore a
  | TitleFilter String a
  | AuthorFilter String a
  | AuthorFacetAdd a
  | AuthorFacetAdd_ Author a
  | AuthorFacetRemove String a
  | YearFilter Year Year a

type ChildQuery = Coproduct3 CA.Query CB.Query CC.Query

type ChildSlot = Either3 Unit Unit Unit

component
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => H.Component HH.HTML Query Unit Void m
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = NotAsked

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render NotAsked = HH.div_
    [ HH.text "NotAsked"
    , HH.button
        [ HE.onClick (HE.input_ RequestArchive) ]
        [ HH.text "Request archive." ]
    ]
  render Loading = HH.text "Loading"
  render (Loaded stateRec) = HH.div_
    [ HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "Component A" ]
        , HH.slot' CP.cp1 unit CA.component unit absurd
        ]
    , HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "Component B" ]
        , HH.slot' CP.cp2 unit CB.component unit absurd
        ]
    , HH.div
        [ HP.class_ (H.ClassName "box")]
        [ HH.h1_ [ HH.text "Component C" ]
        , HH.slot' CP.cp3 unit CC.component unit absurd
        ]
    , HH.p_
        [ HH.text "Last observed states:"]
    , HH.ul_
        [ HH.li_ [ HH.text ("Component A: " <> show (Nothing :: Maybe Boolean)) ]
        , HH.li_ [ HH.text ("Component B: " <> show (Nothing :: Maybe Int)) ]
        , HH.li_ [ HH.text ("Component C: " <> show (Nothing :: Maybe String)) ]
        ]
    , HH.button
        [ HE.onClick (HE.input_ RenderMore) ]
        [ HH.text "Check states now" ]
    ]
  render LoadError = HH.text "LoadError"

  convert :: { archive :: Archive, date :: WrappedDate } -> StateRec
  convert { archive, date } =
    let (WrappedDate _date) = date
    in
    { now: _date
    , authors: archive.authors
    , papers: archive.papers
    , yearMin: archive.yearMin
    , yearMax: archive.yearMax
    , authorFacets: []
    , renderAmount: RenderSome
    , visibleIds: Set.empty
    , filters:
      { title: ""
      , titleIds: Set.empty
      , author: ""
      , authorIds: Set.empty
      , yearMin: toInt archive.yearMin
      , yearMax: toInt archive.yearMax
      }
    }

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (RequestArchive next) = do
     H.put Loading
     maybeResponse <- requestArchive
     H.put $ maybe LoadError (Loaded <<< convert) maybeResponse
     pure next
  eval (DisplayArchive archive date next) = pure next
  eval (RenderMore next) = pure next
  eval (TitleFilter string next) = pure next
  eval (AuthorFilter string next) = pure next
  eval (AuthorFacetAdd next) = pure next
  eval (AuthorFacetAdd_ string next) = pure next
  eval (AuthorFacetRemove string next) = pure next
  eval (YearFilter yearMin yearMax next) = pure next
--  eval (ReadStates next) = do
--    a <- H.query' CP.cp1 unit (H.request CA.GetState)
--    b <- H.query' CP.cp2 unit (H.request CB.GetCount)
--    c <- H.query' CP.cp3 unit (H.request CC.GetValue)
--    H.put Loading
--    pure next
--  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
--  eval (Navigate destination a) = do
--    { route } <- H.get 
--    when (route /= destination) do
--      H.modify_ _ { route = destination }
--    pure a
