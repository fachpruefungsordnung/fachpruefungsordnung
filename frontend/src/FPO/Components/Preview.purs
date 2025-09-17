module FPO.Components.Preview where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Button (Output) as Button
import FPO.Data.Request (LoadState(..))
import Halogen as H
import Halogen.HTML (iframe) as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Web.HTML.Common (AttrName(..))

type Output = Unit

data Action = Receive Input

data Query a = NoQuery a

type Slots = (button :: forall query. H.Slot query Button.Output Int)

type State =
  { renderedHtml :: Maybe (LoadState String)
  , isDragging :: Boolean
  }

type Input = { renderedHtml :: Maybe (LoadState String), isDragging :: Boolean }

preview :: forall m. MonadAff m => H.Component Query Input Output m
preview = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: Input -> State
  initialState { renderedHtml, isDragging } =
    { renderedHtml: renderedHtml
    , isDragging: isDragging
    }

  render :: State -> H.ComponentHTML Action Slots m
  render state =
    HH.iframe
      [ HP.srcDoc $ case state.renderedHtml of
          Nothing -> "No content rendered yet."
          Just (Loaded html) -> html
          Just Loading -> "Loading..."
      , HP.attr (AttrName "height") "100%"
      , HP.attr (AttrName "width") "100%"
      , if state.isDragging then HP.classes [ HB.peNone ] else HP.classes []
      ]

  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots Unit m Unit
  handleAction = case _ of
    Receive { renderedHtml, isDragging } -> do
      H.modify_ _ { renderedHtml = renderedHtml, isDragging = isDragging }
