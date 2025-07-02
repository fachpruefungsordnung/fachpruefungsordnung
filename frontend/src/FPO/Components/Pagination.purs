-- | Generic pagination component.
-- | This component can be used to display a pagination bar with clickable page numbers.
-- |
-- | It supports two `Style`s (`Full` and `Compact`) to control how many
-- | page numbers are displayed. Furthermore, one can specify how the component reacts
-- | to changes in the page count (i.e., when the user changes the amount of
-- | pages and thus the amount of pagination items) using `Reaction`.
-- |
-- | The component allows for querying, i.e., the parent component can
-- | request the component to jump to a specific page number.

module FPO.Components.Pagination
  ( Input
  , Output(..)
  , Query(..)
  , Reaction(..)
  , Style(..)
  , calculatePageCount
  , component
  ) where

import Prelude

import Data.Array ((..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

-- | Calculates the number of pages based on the total number of items
-- | and the number of items per page.
calculatePageCount :: Int -> Int -> Int
calculatePageCount totalItems itemsPerPage =
  max 1 $ totalItems `div` itemsPerPage +
    if totalItems `mod` itemsPerPage > 0 then 1 else 0

type Input = { pages :: Int, style :: Style, reaction :: Reaction }

-- | The style of the pagination component.
data Style
  -- | Full pagination style, showing all page numbers.
  = Full
  -- | Compact pagination style, showing only a few page numbers
  -- | (first, last, current and directly adjacent pages).
  -- |
  -- | The argument `i` in `Compact i` specifies how many adjacent pages
  -- | to show on each side of the current page, excluding the start and end pages
  -- | (which are always shown).
  | Compact Int

-- | Specifies the way the pagination reacts to changes to
-- | the page count (i.e., the user changes the amount of pages
-- | and thus the amount of pagination items).
data Reaction
  -- | The first page is reached.
  = FirstPage
  -- | The closest page is reached.
  -- |
  -- | That is, if the page number increases,
  -- | the shown page does not change. Otherwise, if the current page number
  -- | becomes invalid, the highest valid page number is shown.
  -- TODO: We should perhaps improve the visuals because changing the amount of
  --       pagination items shown changes the layout and makes the buttons jump around,
  --       which is not very user-friendly.
  | PreservePage

-- | The page number that was clicked.
data Output = Clicked Int
type State =
  { active :: Int, totalPages :: Int, style :: Style, reaction :: Reaction }

data Query a = SetPageQ Int a

data Action
  = SetPage Int
  | Receive Input

component
  :: forall m
   . H.Component Query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Input -> State
  initialState input =
    { active: 0
    , totalPages: input.pages
    , style: input.style
    , reaction: input.reaction
    }

  render :: State -> H.ComponentHTML Action () m
  render state =
    case state.style of
      Full -> fullPagination state
      Compact i -> compactPagination i state

  fullPagination :: forall w. State -> HH.HTML w Action
  fullPagination state =
    HH.ul [ HP.classes [ HB.pagination, HB.mt3, HB.justifyContentCenter ] ] $
      range' 0 (state.totalPages - 1) <#> \pageNumber ->
        createPageItem pageNumber (pageNumber == state.active)

  compactPagination :: forall w. Int -> State -> HH.HTML w Action
  compactPagination adjacentPages state =
    let
      adjacentPages' =
        if state.active == 0 || state.active == state.totalPages - 1 then
          adjacentPages + 1
        else adjacentPages
      totalPages = state.totalPages
      activePage = state.active
      firstPage = 0
      lastPage = totalPages - 1
      startPage = max firstPage (activePage - adjacentPages')
      endPage = min lastPage (activePage + adjacentPages')
      pagesToShow = range' startPage endPage
      dotdotdot = HH.li [ HP.classes [ HB.pageItem ] ]
        [ HH.a
            [ HP.classes [ HB.pageLink, HB.disabled ]
            , HP.href "#"
            ]
            [ HH.text "..."
            ]
        ]
    in
      if totalPages <= 5 then
        fullPagination state
      else
        HH.ul
          [ HP.classes [ HB.pagination, HB.mt3, HB.justifyContentCenter ] ]
          $
            ( orEmpty (startPage > firstPage)
                ( createPageItem firstPage
                    (firstPage == activePage)
                )
            )
              <>
                ( orEmpty
                    (startPage > firstPage + 1)
                    dotdotdot
                )
              <>
                ( pagesToShow <#> \pageNumber ->
                    createPageItem pageNumber (pageNumber == activePage)
                )
              <>
                ( orEmpty
                    (endPage < lastPage - 1)
                    dotdotdot
                )
              <>
                ( orEmpty
                    (endPage < lastPage)
                    (createPageItem lastPage (lastPage == activePage))
                )

  createPageItem :: forall w. Int -> Boolean -> HH.HTML w Action
  createPageItem pageNumber enabled =
    HH.li
      [ HP.classes $ [ HB.pageItem ] <> if enabled then [ HB.active ] else [] ]
      [ HH.a
          [ HP.classes [ HB.pageLink ]
          , HP.href "#"
          , HE.onClick $ const (SetPage pageNumber)
          ]
          [ HH.text $ show $ pageNumber + 1
          ]
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    SetPage page -> do
      H.modify_ \state -> state { active = page }
      H.raise $ Clicked page
    Receive input -> do
      state <- H.get
      let
        beforeActive = state.active
        newActive =
          if input.pages /= state.totalPages then
            -- Total page count changed, handle this change appropriately.
            case input.reaction of
              FirstPage -> 0
              PreservePage -> max 0 $ min (input.pages - 1) beforeActive
          else
            -- Total page count did not change, keep the current active page
            beforeActive

      H.modify_ _
        { active = newActive
        , totalPages = input.pages
        , style = input.style
        }

      when (beforeActive /= newActive) do
        -- If the active page force-changed, we raise an event with the new active page.
        -- This is useful for the parent component to react to the change.
        H.raise $ Clicked newActive

  handleQuery
    :: forall a
     . Query a
    -> H.HalogenM State Action () Output m (Maybe a)
  handleQuery = case _ of
    SetPageQ page x -> do
      handleAction (SetPage page)
      pure $ Just x

  -- A range function that is not completely weird, compared to the one in `Data.Array`.
  -- Spans from start to end, inclusive. As you would expect, if the end is smaller
  -- than the start, it returns an **empty** array!
  range' :: Int -> Int -> Array Int
  range' start end
    | start > end = []
    | otherwise = start .. end

  orEmpty :: forall a. Boolean -> a -> Array a
  orEmpty condition value =
    if condition then [ value ] else []
