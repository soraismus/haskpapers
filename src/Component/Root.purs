module HaskPapers.Component.Root
  ( Query
  , component
  ) where

import Prelude

import Data.Array as Array
import Data.Array ((!!))
import Data.Date (Date)
import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Map (Map)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed.InputType as InputType
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Timer (setTimeout)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Core (toPropValue)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (eventSource')
import HaskPapers.Capability.LogMessages (class LogMessages, logDebug)
import HaskPapers.Capability.Now (class Now)
import HaskPapers.Capability.RequestArchive
  ( class RequestArchive
  , requestArchive
  )
import HaskPapers.Component.ComponentA as CA
import HaskPapers.Component.ComponentB as CB
import HaskPapers.Component.ComponentC as CC
import HaskPapers.Component.Utils (getDailyIndex)
import HaskPapers.Data.Archive (Archive)
import HaskPapers.Data.Author (Author)
import HaskPapers.Data.Id (Id)
import HaskPapers.Data.Link (Link)
import HaskPapers.Data.Paper (Paper)
import HaskPapers.Data.Title (Title)
import HaskPapers.Data.ToHtmlString (toHtmlString)
import HaskPapers.Data.Year (Year, toInt)
import HaskPapers.Data.WrappedDate (WrappedDate(..))
import HaskPapers.Foreign.Slider (SliderYears, onSliderUpdate)

data RenderAmount = RenderAll | RenderSome

derive instance genericRenderAmount :: Generic RenderAmount _

instance showRenderAmount :: Show RenderAmount where
  show = genericShow

type StateRec =
  { now :: Date
  , papers :: Array Paper
  , dailyIndex :: Int
  , authors :: Map Id Author
  , overallMinYear :: Year
  , overallMaxYear :: Year
  , authorFacets :: Array { name :: String, titleIds :: Set Id }
  , visibleIds :: Set Id
  , selectedPapers :: Array Paper 
  , filters ::
      { title :: String
      , titleIds :: Set Id
      , author :: String
      , authorIds :: Set Id
      , includeUnknown :: Boolean
      , minYear :: Year
      , maxYear :: Year
      , renderAmount :: RenderAmount
      }
  }

data State
  = NotAsked
  | Loading
  | Loaded StateRec
  | LoadError

derive instance genericState :: Generic State _

instance showState :: Show State where
  show = genericShow

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
  | UpdateAccToSlider SliderYears (H.SubscribeStatus -> a)
  | SetIncludeUnknown Boolean a

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

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (RequestArchive next) = do
    H.put Loading
    requestArchive >>= case _ of
      Just response -> evalResponse response next
      Nothing       -> H.put LoadError *> pure next
  eval (DisplayArchive archive date next) = pure next
  eval (RenderMore next) = do
    H.modify_ $ updateForFilters \stateRec ->
      stateRec.filters { renderAmount = RenderAll }
    pure next
  eval (TitleFilter string next) = pure next
  eval (AuthorFilter string next) = pure next
  eval (AuthorFacetAdd next) = pure next
  eval (AuthorFacetAdd_ string next) = pure next
  eval (AuthorFacetRemove string next) = pure next
  eval (YearFilter minYear maxYear next) = pure next
  eval (UpdateAccToSlider sliderYears@(Tuple minYear maxYear) reply) = do
    logDebug ("UpdateAccToSlider -- " <> show sliderYears)
    H.modify_ $ updateForFilters \stateRec ->
      stateRec.filters { minYear = minYear, maxYear = maxYear }
    pure $ reply H.Listening
  eval (SetIncludeUnknown includeUnknown next) = do
    logDebug $ "SetIncludeUnknown " <> show includeUnknown
    H.modify_ $ updateForFilters \stateRec ->
      stateRec.filters { includeUnknown = includeUnknown }
    pure next

  evalResponse
    :: forall a
    . { archive :: Archive, date :: WrappedDate }
    -> a
    -> H.ParentDSL State Query ChildQuery ChildSlot Void m a
  evalResponse response next = do
    let paperCount = Array.length response.archive.papers
    dailyIndex <- getDailyIndex paperCount
    let stateRec = convert dailyIndex response
    H.put $ Loaded stateRec
    let min = toInt stateRec.overallMinYear
    let max = (toInt stateRec.overallMaxYear) + 1
    let slider = { id: "year-slider"
                 , start: [min, max]
                 , margin: Just 1
                 , limit: Nothing
                 , connect: Just true
                 , direction: Nothing
                 , orientation: Nothing
                 , behavior: Nothing
                 , step: Just 1
                 , range: Just { min, max }
                 }
    logDebug ("RequestArchive -- " <> show slider)
    H.subscribe $ eventSource'
      (onSliderUpdate slider)
      (Just <<< H.request <<< UpdateAccToSlider)
    pure next

convert :: Int -> { archive :: Archive, date :: WrappedDate } -> StateRec
convert index { archive, date: WrappedDate _date } =
  { now: _date
  , authors: archive.authors
  , dailyIndex: index
  , papers: archive.papers
  , overallMinYear: archive.minYear
  , overallMaxYear: archive.maxYear
  , authorFacets: []
  , visibleIds: Set.empty
  , selectedPapers: archive.papers
  , filters:
    { title: ""
    , titleIds: Set.empty
    , author: ""
    , authorIds: Set.empty
    , includeUnknown: false
    , minYear: archive.minYear
    , maxYear: archive.maxYear
    , renderAmount: RenderSome
    }
  }

filter
  :: forall r
   . { includeUnknown :: Boolean
     , minYear :: Year
     , maxYear :: Year
     , renderAmount :: RenderAmount
     | r
     }
  -> Array Paper
  -> Array Paper
filter record@{ renderAmount } = case renderAmount of
    RenderSome -> _filter <<< Array.take 25
    RenderAll  -> _filter
  where
    _filter = Array.filter $ isSelected record

isSelected
  :: forall r
   . { includeUnknown :: Boolean, minYear :: Year, maxYear :: Year | r }
  -> Paper
  -> Boolean
isSelected { includeUnknown, minYear, maxYear } paper =
  maybe
    includeUnknown
    (\year -> year >= minYear && year <= maxYear)
    paper.yearMaybe

mapState :: (StateRec -> StateRec) -> State -> State
mapState f (Loaded stateRec) = Loaded $ f stateRec
mapState f state             = state

updateForFilters
  :: (  StateRec
     -> { title :: String
        , titleIds :: Set Id
        , author :: String
        , authorIds :: Set Id
        , includeUnknown :: Boolean
        , minYear :: Year
        , maxYear :: Year
        , renderAmount :: RenderAmount
        }
     )
  -> State
  -> State
updateForFilters getFilters state =
  mapState
    (\stateRec ->
      let
        filters = getFilters stateRec
      in
        stateRec
          { selectedPapers = filter filters stateRec.papers
          , filters = filters
          })
    state

view
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => StateRec
  -> H.ParentHTML Query ChildQuery ChildSlot m
view stateRec@{ dailyIndex, papers, selectedPapers } =
  HH.div
    [ class_ "container" ]
    [ viewHeader $ Array.length selectedPapers
    , HH.button
      [ class_ "btn btn-outline-danger"
      , HE.onClick (HE.input_ RenderMore)
      ]
      [ HH.text "Render more" ]
    , viewPaperOfTheDay dailyIndex papers
    , viewFilters stateRec
    , viewPapers stateRec
    ]

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
    [ HH.h1_
      [ HH.text
          $  show n
          <> " Haskell Paper"
          <> if n == 1 then "" else "s"
      ]
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
    , viewIncludeUnknown
    , HH.div [ HP.id_ "year-slider" ] []
    ]

viewIncludeUnknown
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => H.ParentHTML Query ChildQuery ChildSlot m
viewIncludeUnknown = HH.div
  [ class_ "include-unknown" ]
  [ HH.input
    [ class_ "include-unknown-checkbox"
    , HP.type_ InputType.InputCheckbox
    , HE.onChecked (HE.input SetIncludeUnknown)
    ]
  , HH.text "Include papers with unknown year of publication "
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
      , HP.placeholder "Search titles"
      , HP.type_ InputType.InputText
      , HP.value filter
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
      , HP.placeholder "Search authors"
      , HP.type_ InputType.InputText
      , HP.value filter
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
  => { selectedPapers :: Array Paper
     , filters
         :: { title :: String
            , author :: String
            , minYear :: Year
            , maxYear :: Year
            , renderAmount :: RenderAmount
            | r1
            }
     | r0
     }
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewPapers { selectedPapers, filters } =
  HH.ul
    [ class_ "paper-list" ]
    (map (viewPaper filters) selectedPapers)

viewPaper
  :: forall m r
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => { title :: String, author :: String | r }
  -> Paper
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewPaper { author, title } paper =
  HH.li
    [ class_ "paper" ]
    [ viewTitle paper.title (Array.head paper.links) (Just title)
    , HH.p
      [ class_ "details" ]
      [ viewAuthors paper.authors (Just author)
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
