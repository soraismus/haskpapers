module HaskPapers.Component.Root
  ( Query
  , component
  ) where

import Prelude

import Web.UIEvent.KeyboardEvent (KeyboardEvent, key)
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Data.Array ((!!))
import Data.Array as Array
import Data.Date (Date)
import Data.Either (Either(..), hush, either)
import Data.Either.Nested (Either3)
import Data.Filterable (filterMap, maybeBool)
import Data.Foldable (all, any, foldM, foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Functor.Coproduct.Nested (Coproduct3)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (ignoreCase)
import Data.Tuple (Tuple(..))
import DOM.HTML.Indexed.InputType as InputType
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Core (toPropValue)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource (eventSource', eventSource_')
import HaskPapers.Capability.LogMessages (class LogMessages, logDebug)
import HaskPapers.Capability.Now (class Now)
import HaskPapers.Capability.RequestArchive
  ( class RequestArchive
  , requestArchive
  )
import HaskPapers.Component.ComponentA as CA
import HaskPapers.Component.ComponentB as CB
import HaskPapers.Component.ComponentC as CC
import HaskPapers.Component.Utils
  ( afterDuration
  , deleteWhen
  , getDailyIndex
  , inArray
  )
import HaskPapers.Component.ViewUtils (class_, padLeft3)
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
  , ids :: Set Id
  , dailyIndex :: Int
  , authors :: Map Id Author
  , authorsIndex :: Map Id (Set Id)
  , overallMinYear :: Year
  , overallMaxYear :: Year
  , selectedPapers :: Array Paper 
  , filters ::
      { title :: String
      , idsForTitle :: Set Id
      , author :: String
      , facets :: Array { name :: String, titleIds :: Set Id }
      , idsForAuthor :: Set Id
      , includeUnknown :: Boolean
      , minYear :: Year
      , maxYear :: Year
      , renderAmount :: RenderAmount
      , isIrrelevant :: { title :: Boolean
                        , author :: Boolean
                        , facet :: Boolean
                        }
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
  | RenderMore (H.SubscribeStatus -> a)
  | FilterByTitle String a
  | FilterByAuthor String a
  | AddFacet Author a
  | AddFacetFromFilter a
  | RemoveFacet String a
  | FilterByYear SliderYears (H.SubscribeStatus -> a)
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
        [ HE.onClick $ HE.input_ RequestArchive ]
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
  eval (RenderMore reply) = do
    logDebug "RenderMore"
    H.modify_ $ updateForFilters \stateRec ->
      stateRec.filters { renderAmount = RenderAll }
    pure $ reply H.Done
  eval (FilterByTitle string next) = do
    logDebug $ "FilterByTitle: " <> string
    H.modify_ $ updateForFilters \stateRec ->
      stateRec.filters
        { title = string
        , idsForTitle = getIdsForTitle stateRec.ids string stateRec.papers
        , isIrrelevant { title = String.null $ String.trim string }
        }
    pure next
  eval (FilterByAuthor string next) = do
    logDebug $ "FilterByAuthor: " <> string
    H.modify_ $ updateForFilters \stateRec ->
      stateRec.filters
        { author = string
        , idsForAuthor = getIdsForAuthor stateRec.ids string stateRec.papers
        , isIrrelevant { author = String.null $ String.trim string }
        }
    pure next
  eval (AddFacetFromFilter next) = do
    logDebug "AddFacetFromFilter"
    H.modify_ $ updateForFilters \stateRec ->
      let
        filters = stateRec.filters
        name = filters.author
        ids = filters.idsForAuthor
        facets = filters.facets
      in
        if String.null name
          then filters
          else filters { author = ""
                       , idsForAuthor = Set.empty
                       , facets = getFacets name ids facets
                       , isIrrelevant { author = true, facet = false }
                       }
    pure next
  eval (AddFacet author next) = do
    logDebug $ "AddFacet: " <> show author
    H.modify_ $ updateForFilters \stateRec ->
      let
        authors = stateRec.authors
        titles = stateRec.authorsIndex
        filters = stateRec.filters
        name = filters.author
        ids = filters.idsForAuthor
        facets = filters.facets
      in
        if inArray (toHtmlString author) $ map _.name facets
          then filters
          else filters
            { facets =
                Array.snoc
                  facets
                  { name: toHtmlString author
                  , titleIds: buildAuthorFilterIds author authors titles
                  }
            , isIrrelevant { facet = false }
            }
    pure next
  eval (RemoveFacet string next) = do
    logDebug $ "RemoveFacet: " <> string
    H.modify_ $ updateForFilters \stateRec ->
      let
        filters = stateRec.filters
        facets = filters.facets
      in if inArray string $ map _.name facets
        then filters
          { facets = deleteWhen ((_ == string) <<< _.name) facets
          , isIrrelevant { facet = Array.length facets == 1 }
          }
        else filters
    pure next
  eval (FilterByYear sliderYears@(Tuple minYear maxYear) reply) = do
    logDebug $ "FilterByYear -- " <> show sliderYears
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
      (Just <<< H.request <<< FilterByYear)
    H.subscribe $ eventSource_'
      (afterDuration 750)
      (H.request RenderMore)
    pure next

getFacets
  :: String
  -> Set Id
  -> Array { name :: String, titleIds :: Set Id }
  -> Array { name :: String, titleIds :: Set Id }
getFacets name ids facets =
  if inArray name $ map _.name facets
    then facets
    else Array.snoc facets { name: name, titleIds: ids }

buildAuthorFilterIds
  :: Author
  -> Map Id Author
  -> Map Id (Set Id)
  -> Set Id
buildAuthorFilterIds author authors authorsIndex
  | String.null $ String.trim $ toHtmlString author = Set.empty
  | otherwise =
      either (const Set.empty) identity do
        regexes <- getRegexes $ toHtmlString author
        let _reduce = reduce regexes
        pure $ foldrWithIndex _reduce Set.empty authors
  where
    _getRegexes :: Array String -> Either String (List Regex)
    _getRegexes =
      foldM
        (\regexes str -> case Regex.regex str ignoreCase of
            Left error -> Left $ "Error on '" <> str <> "': " <> error
            Right regex -> Right $ (regex : regexes))
        Nil

    getRegexes :: String -> Either String (List Regex)
    getRegexes = _getRegexes <<< split

    -- NB: I think this is intended for a binominal/trinominal author.
    match :: List Regex -> String -> Boolean
    match regexes fullName =
      all (\regex -> Regex.test regex fullName) regexes

    reduce :: List Regex -> Id -> Author -> Set Id -> Set Id
    reduce regexes id author ids =
      if match regexes $ toHtmlString author
        then case Map.lookup id authorsIndex of
          Nothing -> ids
          Just _ids -> Set.union ids _ids
        else ids

getIdsForTitle :: Set Id -> String -> Array Paper -> Set Id
getIdsForTitle ids str papers
  | String.null $ String.trim str = ids
  | otherwise =
      either (const Set.empty) identity do
        regexes <- getRegexes str
        let _reduce = reduce regexes
        pure $ foldr _reduce Set.empty papers
  where
    _getRegexes :: Array String -> Either String (List Regex)
    _getRegexes =
      foldM
        (\regexes str -> case Regex.regex str ignoreCase of
            Left error -> Left $ "Error on '" <> str <> "': " <> error
            Right regex -> Right $ (regex : regexes))
        Nil

    getRegexes :: String -> Either String (List Regex)
    getRegexes = _getRegexes <<< split

    reduce :: List Regex -> Paper -> Set Id -> Set Id
    reduce regexes paper ids =
      let str = toHtmlString paper.title
      in if all (\regex -> Regex.test regex str) regexes
           then Set.insert paper.titleId ids
           else ids

getIdsForAuthor :: Set Id -> String -> Array Paper -> Set Id
getIdsForAuthor ids str papers
  | String.null $ String.trim str = ids
  | otherwise =
      either (const Set.empty) identity do
        regexes <- getRegexes str
        let _reduce = reduce regexes
        pure $ foldr _reduce Set.empty papers
  where
    _getRegexes :: Array String -> Either String (List Regex)
    _getRegexes =
      foldM
        (\regexes str -> case Regex.regex str ignoreCase of
            Left error -> Left $ "Error on '" <> str <> "': " <> error
            Right regex -> Right $ (regex : regexes))
        Nil

    getRegexes :: String -> Either String (List Regex)
    getRegexes = _getRegexes <<< split

    match :: List Regex -> Array Author -> Boolean
    match regexes authors =
      all
        (\regex ->
          any
            (\author -> Regex.test regex $ toHtmlString author)
            authors)
        regexes

    reduce :: List Regex -> Paper -> Set Id -> Set Id
    reduce regexes paper ids =
      if match regexes paper.authors
        then Set.insert paper.titleId ids
        else ids

getAllIds :: Array Paper -> Set Id
getAllIds = foldr (Set.insert <<< _.titleId) Set.empty

convert :: Int -> { archive :: Archive, date :: WrappedDate } -> StateRec
convert index { archive, date: WrappedDate _date } =
  let
    ids = getAllIds archive.papers
  in
    { now: _date
    , authors: archive.authors
    , authorsIndex: archive.authorsIndex
    , dailyIndex: index
    , papers: archive.papers
    , ids
    , overallMinYear: archive.minYear
    , overallMaxYear: archive.maxYear
    , selectedPapers: archive.papers
    , filters:
      { title: ""
      , idsForTitle: ids
      , author: ""
      , facets: []
      , idsForAuthor: ids
      , includeUnknown: true
      , minYear: archive.minYear
      , maxYear: archive.maxYear
      , renderAmount: RenderSome
      , isIrrelevant: { title: true
                      , author: true
                      , facet: true
                      }
      }
    }

filter
  :: forall r0 r1
   . { idsForAuthor :: Set Id
     , idsForTitle :: Set Id
     , facets :: Array { titleIds :: Set Id | r0 }
     , includeUnknown :: Boolean
     , minYear :: Year
     , maxYear :: Year
     , renderAmount :: RenderAmount
     , isIrrelevant :: { title :: Boolean
                       , author :: Boolean
                       , facet :: Boolean
                       }
     | r1
     }
  -> Array Paper
  -> Array Paper
filter record@{ renderAmount } = case renderAmount of
    RenderSome -> _filter <<< Array.take 25
    RenderAll  -> _filter
  where
    _filter = Array.filter $ isSelected record

isSelected
  :: forall r0 r1
   . { idsForAuthor :: Set Id
     , idsForTitle :: Set Id
     , facets :: Array { titleIds :: Set Id | r0 }
     , includeUnknown :: Boolean
     , minYear :: Year
     , maxYear :: Year
     , isIrrelevant :: { title :: Boolean
                       , author :: Boolean
                       , facet :: Boolean
                       }
     | r1
     }
  -> Paper
  -> Boolean
isSelected filters@{ facets, includeUnknown, minYear, maxYear } paper =
  let
    id = paper.titleId

    isIrrelevant = filters.isIrrelevant

    isYearSelected = maybe includeUnknown
      (\year -> year >= minYear && year <= maxYear)
      paper.yearMaybe
  in
    isYearSelected
      && (isIrrelevant.title || Set.member id filters.idsForTitle)
      && (isIrrelevant.author || Set.member id filters.idsForAuthor)
      && (isIrrelevant.facet ||  all (Set.member id <<< _.titleIds) facets)

mapState :: (StateRec -> StateRec) -> State -> State
mapState f (Loaded stateRec) = Loaded $ f stateRec
mapState f state             = state

updateForFilters
  :: (  StateRec
     -> { title :: String
        , idsForTitle :: Set Id
        , author :: String
        , facets :: Array { name :: String, titleIds :: Set Id }
        , idsForAuthor :: Set Id
        , includeUnknown :: Boolean
        , minYear :: Year
        , maxYear :: Year
        , renderAmount :: RenderAmount
        , isIrrelevant :: { title :: Boolean
                          , author :: Boolean
                          , facet :: Boolean
                          }
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
    , viewPaperOfTheDay dailyIndex papers
    , viewFilters stateRec.filters
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
  :: forall m r
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => { title :: String
     , author :: String
     , facets :: Array { name :: String, titleIds :: Set Id }
     | r
     }
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewFilters { title, author, facets } =
  HH.p_
    [ viewTitleSearchBox title
    , viewAuthorSearchBox author
    , viewFacets $ map _.name facets
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
    , HP.checked true
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
      , HE.onValueInput $ HE.input FilterByTitle
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
viewAuthorSearchBox authorName =
  HH.div
    [ class_ "author-search" ]
    [ HH.input
      [ class_ "author-search-box"
      , HP.placeholder "Search authors"
      , HP.type_ InputType.InputText
      , HP.value authorName
      , HE.onValueInput $ HE.input $ FilterByAuthor
      , HE.onKeyUp $ (maybeBool isEnter >.> AddFacetFromFilter)
      ]
    ]

isEnter :: KeyboardEvent -> Boolean
isEnter keyboardEvent = (key keyboardEvent) == "Enter"

pushMap
  :: forall f g a b
   . Functor f
  => (a -> f b)
  -> (forall c. c -> g c)
  -> a
  -> f (g Unit)
pushMap f g = f >>> \fb -> map (const $ g unit) fb

infixr 5 pushMap as >.>

viewFacet
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => String
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewFacet str =
  HH.div
    [ class_ "facet"
    , HE.onClick $ HE.input_ $ RemoveFacet str
    ]
    [ HH.text str ]

viewFacets
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Array String
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewFacets strs =
  HH.div
    [ class_ "facets" ]
    (map viewFacet strs)

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
    linkNodes
  where
    titleNodes = maybe
      [HH.text $ toHtmlString title]
      (\filter -> highlightPatches (split filter) $ toHtmlString title)
      maybeFilter
    linkNodes = maybe
      titleNodes
      (\link -> [HH.a [class_ "link", HP.href $ toHtmlString link] titleNodes])
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
  HH.a
    [ class_ "subtle-link edit"
    , HP.href editLink
    ]
    [ HH.text "(edit)" ]
  where
    editLink =
      "https://github.com"
        <> "/mitchellwrosen/haskell-papers"
        <> "/edit/master/papers"
        <> (padLeft3 $ show file)
        <> ".yaml#L"
        <> show line

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
  let
    str = toHtmlString author
  in
    HH.span
      [ class_ "author"
      , HE.onClick $ HE.input_ $ AddFacet author
      ]
      (maybe
        [HH.text str]
        (\filter -> highlightPatches (split filter) str)
        maybeFilter)

viewYearMaybe
  :: forall m r
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Maybe Year
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewYearMaybe maybeYear =
  maybe
    (HH.text "")
    (\year -> HH.text $ " [" <> toHtmlString year <> "] ")
    maybeYear

viewCitations
  :: forall m r
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Array Title
  -> H.ParentHTML Query ChildQuery ChildSlot m
viewCitations citations =
  HH.text _text
  where
    count = Array.length citations
    _text = if count == 0 then "" else " (cited by " <> show count <> ")"

renderSegment
  :: forall m r
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Either NonMatch Match
  -> H.ParentHTML Query ChildQuery ChildSlot m
renderSegment =
  either
    HH.text
    (HH.span [ class_ "highlight" ] <<< Array.singleton <<< HH.text)

toArray :: forall a. List a -> Array a
toArray list = Array.fromFoldable list

toList :: forall a. Array a -> List a
toList array = List.fromFoldable array

highlightPatches
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => RequestArchive m
  => Array String
  -> String
  -> Array (H.ParentHTML Query ChildQuery ChildSlot m)
highlightPatches needles haystack =
  toArray $ map renderSegment segments
  where
    getSegments :: List String -> String -> List (Either NonMatch Match)
    getSegments =
      explode
        <<< (foldr insertInterval Nil)
        <<< (filterMap ((_ $ haystack) <<< interval))

    segments :: List (Either NonMatch Match)
    segments = getSegments (toList needles) haystack

interval :: String -> String -> Maybe (Tuple Int Int)
interval str0 str1
  | String.null str0 = Nothing
  | otherwise = do
      regex <- hush $ Regex.regex str0 ignoreCase
      index <- Regex.search regex str1
      pure $ Tuple index (index + String.length str0)

type Interval a = Tuple a a

insertInterval
  :: forall a
   . Ord a
  => Interval a
  -> List (Interval a)
  -> List (Interval a)
insertInterval x_to_y@(Tuple x y) = case _ of
  Nil -> (Tuple x y) : Nil
  Cons (Tuple x' y') zs
    | y < x'    -> (Tuple x y) : (Tuple x' y') : zs
    | y' < x    -> (Tuple x' y') : insertInterval x_to_y zs
    | otherwise -> (Tuple (min x x') (max y y')) : zs

type NonMatch = String
type Match = String

_slice :: Int -> Int -> String -> String
_slice i j str = fromMaybe "" $ SCU.slice i j str

explode
  :: List (Tuple Int Int)
  -> String
  -> List (Either NonMatch Match)
explode intervals str = go 0 intervals
  where
  go :: Int -> List (Tuple Int Int) -> List (Either NonMatch Match)
  go n = case _ of
    Nil ->
      let
        length = String.length str
      in
        if n == length
          then Nil
          else (Left $ _slice n length str) : Nil

    (Cons (Tuple i j) tail) ->
      if n < i
        then (Left  $ _slice n i str)
               : (Right $ _slice i j str)
               : (go j tail)
        else (Right $ _slice i j str)
             : (go j tail)

_split :: String -> Array String
_split str
  | String.null str = []
  | otherwise       = String.split (Pattern " ") str

split :: String -> Array String
split = _split <<< String.trim
