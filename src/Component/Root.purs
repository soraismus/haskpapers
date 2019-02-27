module HaskPapers.Component.Root
  ( Query
  , component
  ) where

import Prelude

import Data.Array as Array
import Data.Array ((!!))
import Data.Date (Date)
import Data.Either.Nested (Either3)
import Data.Foldable (foldl)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Random (randomInt)
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
import HaskPapers.Data.Link (Link)
import HaskPapers.Data.Paper (Paper)
import HaskPapers.Data.Title (Title)
import HaskPapers.Data.ToHtmlString (toHtmlString)
import HaskPapers.Data.Year (Year, toInt)
import HaskPapers.Data.WrappedDate (WrappedDate(..))

data RenderAmount = RenderAll | RenderSome

type StateRec =
  { now :: Date
  , papers :: Array Paper
  , dailyIndex :: Int
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
  render (Loaded stateRec) = view stateRec
  render LoadError = HH.text "LoadError"

  convert :: Int -> { archive :: Archive, date :: WrappedDate } -> StateRec
  convert index { archive, date: WrappedDate _date } =
    { now: _date
    , authors: archive.authors
    , dailyIndex: index
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
    randomIndex <- H.liftEffect $ randomInt 0 5
    H.put $ maybe LoadError (Loaded <<< convert randomIndex) maybeResponse
    pure next
  eval (DisplayArchive archive date next) = pure next
  eval (RenderMore next) = do
    H.modify_ (\state -> case state of
      Loaded stateRec -> Loaded (stateRec { renderAmount = RenderAll })
      otherwise -> state)
    pure next
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

view
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => StateRec
  -> H.ParentHTML Query ChildQuery ChildSlot m
view stateRec@{ dailyIndex, papers, visibleIds } =
  HH.div
    [ class_ "container" ]
    [ viewHeader visibleCount
    , HH.button
      [ class_ "btn btn-outline-danger"
      , HE.onClick (HE.input_ RenderMore)
      ]
      [ HH.text "Render more" ]
    , viewPaperOfTheDay dailyIndex papers
    , viewFilters stateRec
    , viewPapers stateRec
    ]
  where
    visibleCount = foldl
      (\count paper ->
        if Set.member paper.titleId visibleIds
          then count + 1
          else count)
      0
      papers

viewHeader
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Int
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewHeader n =
  HH.header_
    [ HH.h1_ [ HH.text $ " Haskell Paper" <> if n == 1 then "" else "s" ]
    , HH.a
      [ class_ "sutble-link"
      , HP.href "https://github.com/mitchellwrosen/haskell-papers"
      ]
      [ HH.div_ [ HH.text "contribute on GitHub" ]]
    ]

viewFilters
  :: forall m r0 r1
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => { authorFacets :: Array { name :: String, titleIds :: Set Id }
     , filters :: { title :: String, author :: String | r1 }
     | r0
     }
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewFilters { authorFacets, filters } =
  HH.p_
    [ viewTitleSearchBox filters.title
    , viewAuthorSearchBox filters.author
    , viewAuthorFacets $ map _.name authorFacets
    , HH.div [ HP.id_ "year-slider" ] []
    ]

viewTitleSearchBox
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => String
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewTitleSearchBox filter =
  HH.div
    [ class_ "title-search" ]
    [ HH.input
      [ class_ "title-search-box"
      , HP.value filter
      , HP.placeholder "Search titles"
      ]
    ]

viewAuthorSearchBox
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => String
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewAuthorSearchBox filter =
  HH.div
    [ class_ "author-search" ]
    [ HH.input
      [ class_ "author-search-box"
      , HP.value filter
      , HP.placeholder "Search authors"
      ]
    ]

viewAuthorFacet
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => String
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewAuthorFacet facet =
  HH.div
    [ class_ "facet"
    --, HE.onClick
    ]
    [ HH.text facet ]

viewAuthorFacets
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Array String
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewAuthorFacets authorFacets =
  HH.div
    [ class_ "facets" ]
    (map viewAuthorFacet authorFacets)

viewPaperOfTheDay
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Int
  -> Array Paper
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewPaperOfTheDay index papers =
  maybe
    (HH.div_ [])
    _viewPaperOfTheDay
    (papers !! index)

_viewPaperOfTheDay
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Paper
  -> H.ParentHTML Query ChildQuery ChildSlot m
_viewPaperOfTheDay paper = do
  HH.div_
    [ HH.h3_ [ HH.text "Paper of the Day" ]
    , HH.div
      [ class_ "paper" ]
      [ viewTitle paper.title (Array.head paper.links) Nothing
      , HH.p
        [ class_ "details" ]
        [ viewAuthors paper.authors Nothing
        , viewYearMaybe paper.yearMaybe
        , viewCitations paper.citations
        ]
      , viewEditLink paper.loc
      ]
    ]

viewPapers
  :: forall m r0 r1
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => { papers :: Array Paper
     , renderAmount :: RenderAmount
     , visibleIds :: Set Id
     , filters :: { title :: String, author :: String | r1 }
     | r0
     }
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewPapers record =
  HH.ul
    [ class_ "paper-list" ]
    (map (viewPaper record) (select record.papers))
  where
    select :: Array Paper -> Array Paper
    select = case record.renderAmount of
      RenderAll  -> identity
      RenderSome -> Array.slice 0 25

viewPaper
  :: forall m r0 r1
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => { visibleIds :: Set Id
     , filters :: { title :: String, author :: String | r1 }
     | r0
     }
  -> Paper
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewPaper { filters, visibleIds } paper =
  if Set.member paper.titleId visibleIds
  then HH.text ""
  else
    HH.li
      [ class_ "paper" ]
      [ viewTitle paper.title (Array.head paper.links) (Just filters.title)
      , HH.p
        [ class_ "details" ]
        [ viewAuthors paper.authors (Just filters.author)
        , viewYearMaybe paper.yearMaybe
        , viewCitations paper.citations
        ]
      , viewEditLink paper.loc
      ]

viewTitle
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Title
  -> Maybe Link
  -> Maybe String
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewTitle title maybeLink maybeFilter =
  HH.p
    [ class_ "title" ]
    [ linkNode ]
  where
    titleNode = maybe
      (HH.text $ toHtmlString title)
      (\filter -> HH.text $ toHtmlString title)
      maybeFilter
    linkNode = maybe
      titleNode
      (\link -> HH.a [class_ "link", HP.href $ toHtmlString link] [titleNode])
      maybeLink

viewEditLink
  :: forall m r
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => { file :: Int, line :: Int | r }
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewEditLink { file, line } =
  HH.a [class_ "subtle-link edit", HP.href editLink] [HH.text "(edit)"]
  where
    editLink =
      "https://github.com"
        <> "/mitchellwrosen/haskell-papers"
        <> "/edit/master/papers"
        <> (padLeft3 $ show file)
        <> ".yaml#L"
        <> show line

padLeft3 :: String -> String
padLeft3 str
  | String.length str == 0 = "000"
  | String.length str == 1 = "00" <> str
  | String.length str == 2 = "0" <> str
  | otherwise       = str

-- NB: parentheses, not braces
class_ :: forall r i. String -> HH.IProp ( class :: String | r ) i
class_ = HP.class_ <<< HH.ClassName

viewAuthors
  :: forall m r
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Array Author
  -> Maybe String
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewAuthors authors maybeFilter =
  HH.span_ $ map (viewAuthor maybeFilter) authors

viewAuthor
  :: forall m r
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Maybe String
  -> Author
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewAuthor maybeFilter author =
  HH.span
    [ class_ "author"
    --, HE.onClick
    ]
    [ (maybe (HH.text "") (\filter -> HH.text filter) maybeFilter) ]
-- applyLiveFilterStyle

viewYearMaybe
  :: forall m r
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Maybe Year
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewYearMaybe maybeYear =
  maybe (HH.text "") (\year -> HH.text $ " [" <> toHtmlString year <> "] ") maybeYear

viewCitations
  :: forall m r
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Array Title
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewCitations citations =
  HH.text $ " (cited by " <> show (Array.length citations) <> ")"

--  render (Loaded stateRec) = HH.div_
--    [ HH.div
--        [ HP.class_ (H.ClassName "box")]
--        [ HH.h1_ [ HH.text "Component A" ]
--        , HH.slot' CP.cp1 unit CA.component unit absurd
--        ]
--    , HH.div
--        [ HP.class_ (H.ClassName "box")]
--        [ HH.h1_ [ HH.text "Component B" ]
--        , HH.slot' CP.cp2 unit CB.component unit absurd
--        ]
--    , HH.div
--        [ HP.class_ (H.ClassName "box")]
--        [ HH.h1_ [ HH.text "Component C" ]
--        , HH.slot' CP.cp3 unit CC.component unit absurd
--        ]
--    , HH.p_
--        [ HH.text "Last observed states:"]
--    , HH.ul_
--        [ HH.li_ [ HH.text ("Component A: " <> show (Nothing :: Maybe Boolean)) ]
--        , HH.li_ [ HH.text ("Component B: " <> show (Nothing :: Maybe Int)) ]
--        , HH.li_ [ HH.text ("Component C: " <> show (Nothing :: Maybe String)) ]
--        ]
--    , HH.button
--        [ HE.onClick (HE.input_ RenderMore) ]
--        [ HH.text "Check states now" ]
--    ]

--  render (Loaded stateRec) = HH.div_
--    [ viewHeader 2
--    , HH.div
--        [ class_ "box"]
--        [ HH.h1_ [ HH.text "Component A" ]
--        , HH.slot' CP.cp1 unit CA.component unit absurd
--        ]
--    , HH.div
--        [ class_ "box"]
--        [ HH.h1_ [ HH.text "Component B" ]
--        , HH.slot' CP.cp2 unit CB.component unit absurd
--        ]
--    , HH.div
--        [ class_ "box"]
--        [ HH.h1_ [ HH.text "Component C" ]
--        , HH.slot' CP.cp3 unit CC.component unit absurd
--        ]
--    , HH.p_
--        [ HH.text "Last observed states:"]
--    , HH.ul_
--        [ HH.li_ [ HH.text ("Component A: " <> show (Nothing :: Maybe Boolean)) ]
--        , HH.li_ [ HH.text ("Component B: " <> show (Nothing :: Maybe Int)) ]
--        , HH.li_ [ HH.text ("Component C: " <> show (Nothing :: Maybe String)) ]
--        ]
--    , HH.button
--        [ HE.onClick (HE.input_ RenderMore) ]
--        [ HH.text "Check states now" ]
--    ]
