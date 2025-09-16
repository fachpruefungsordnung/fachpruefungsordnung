-- | Generic table head component.
--   Allows for sorting by clicking on the header.

module FPO.Components.Table.Head
  ( Order(..)
  , Output(..)
  , SortingStyle(..)
  , Title
  , component
  , createTableColumns
  , sortByF
  , toggleSorting
  ) where

import Prelude

import Data.Array (findIndex, sortBy, updateAt, (!!))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

data SortingStyle = Alpha | Numeric
data Order = Asc | Desc
type Title = String

-- TODO: `sorting` should be part of the `Maybe`, because not all columns
--       necessarily have a sorting style. Doesnt really matter because of the default
--       initial state, but it would be a more sensible design.
type Column = { title :: Title, style :: Maybe SortingStyle, order :: Order }

type Input = { columns :: Array Column, sortedBy :: String }

-- | Output type for the component, which emits
-- | the clicked column title and its sorting state.
data Output = Clicked Title Order
type State = { active :: Title, cols :: Array Column }
data Action = HandlePress Title

-- | Creates the initial columns for the table head, with all columns set to ascending
-- | order by default.
createTableColumns
  :: Array { title :: Title, style :: Maybe SortingStyle } -> Array Column
createTableColumns = map (\{ title, style } -> { title, style, order: Asc })

-- | Sorts arrays based on the provided sorting order and sorting function.
sortByF
  :: forall a
   . Order
  -> (a -> a -> Ordering)
  -> Array a
  -> Array a
sortByF sorting f arr =
  case sorting of
    Asc -> sortBy f arr
    Desc -> sortBy (flip f) arr

-- | Switches the sorting direction.
toggleSorting :: Order -> Order
toggleSorting sorting = case sorting of
  Asc -> Desc
  Desc -> Asc

component :: forall query m. H.Component query Input Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        }
    }
  where
  initialState :: Input -> State
  initialState input = { active: input.sortedBy, cols: input.columns }

  render :: State -> H.ComponentHTML Action () m
  render state = HH.thead
    [ HP.classes [ HB.textCenter ] ]
    [ HH.tr [ HP.class_ (HB.tableSecondary) ] $
        map (renderColumn state) state.cols
    ]

  renderColumn :: State -> Column -> H.ComponentHTML Action () m
  renderColumn state column =
    HH.th
      [ HP.classes [ getColumnStyle state column ] ]
      [ case column.style of
          Just sortingStyle ->
            HH.div
              [ HE.onClick $ const (HandlePress column.title)
              , HP.style "display: inline-flex; cursor: pointer;"
              ]
              [ HH.i
                  [ HP.classes
                      [ H.ClassName $ getColumnIcon sortingStyle column.order
                      , HB.me1
                      ]
                  ]
                  []
              , HH.text column.title
              ]
          Nothing ->
            HH.div
              [ HP.style "display: inline-flex; cursor: default;" ]
              [ HH.text column.title
              ]
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    HandlePress title -> do
      state <- H.get

      toggleResult <- pure $ do
        -- Find the index of the column with the given title.
        mcol <- findIndex
          (\col -> col.title == title)
          state.cols
        -- Update it's sorting state.
        elem <- toggleColumnSorting <$> state.cols !! mcol
        -- Update the column in the array.
        cols <- updateAt
          mcol
          elem
          state.cols
        pure $ Tuple cols elem

      case toggleResult of
        Nothing ->
          -- Didn't find the column, do nothing.
          pure unit
        Just (Tuple cols column) -> do
          -- Emit the clicked column title and its sorting state.
          H.raise $ Clicked column.title column.order

          H.modify_ _ { active = title, cols = cols }
          pure unit

  -- Builds the bi-icon class for the respective column.
  getColumnIcon :: SortingStyle -> Order -> String
  getColumnIcon style sorting = "bi-sort-" <> sstyle <> "-" <> sdirection
    where
    sdirection = case sorting of
      Asc -> "down"
      Desc -> "up"
    sstyle = case style of
      Alpha -> "alpha"
      Numeric -> "numeric"

  getColumnStyle :: State -> Column -> H.ClassName
  getColumnStyle state column =
    if state.active == column.title then HB.bgSecondarySubtle
    else HB.bgBodySecondary

  -- Toggles the sorting state of a column.
  toggleColumnSorting :: Column -> Column
  toggleColumnSorting column =
    column { order = toggleSorting column.order }
