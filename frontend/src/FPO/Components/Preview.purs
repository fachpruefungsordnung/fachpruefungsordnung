module FPO.Components.Preview where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect.Aff.Class (class MonadAff)
import FPO.Components.Button (Output) as Button
import FPO.UI.HTML (setInnerHtml)
import Halogen (RefLabel(..), getHTMLElementRef)
import Halogen as H
import Halogen.HTML (div, text) as HH
import Halogen.HTML.Properties (ref)

type Output = Unit

data Action
  = Initialize
  | Receive Input

data Query a = NoQuery a

type Slots = (button :: forall query. H.Slot query Button.Output Int)

type State =
  { renderedHtml :: Maybe String
  }

type Input = { renderedHtml :: Maybe String }

preview :: forall m. MonadAff m => H.Component Query Input Output m
preview = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: Input -> State
  initialState { renderedHtml } =
    { renderedHtml: renderedHtml }

  render :: State -> H.ComponentHTML Action Slots m
  render _ =
    HH.div [ ref (RefLabel "injectHtml") ] [ HH.text "No content rendered yet." ]

  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots Unit m Unit
  handleAction = case _ of
    Initialize -> do
      -- On initialization, we check if there is already rendered HTML in the state
      -- and inject it into the HTML element if it exists.
      -- This is useful when we open the preview component again after it has been rendered before.
      state <- H.get
      let renderedHtml = state.renderedHtml
      htmlElementRef <- getHTMLElementRef (RefLabel "injectHtml")
      case htmlElementRef of
        Just ref -> do
          case renderedHtml of
            Just htmlContent -> do
              H.liftEffect $ setInnerHtml ref htmlContent
              pure unit
            Nothing -> pure unit
        Nothing -> pure unit

    Receive { renderedHtml } -> do
      htmlElementRef <- getHTMLElementRef (RefLabel "injectHtml")
      case htmlElementRef of
        Just ref -> do
          case renderedHtml of
            Just htmlContent -> do
              currentState <- H.get
              if currentState.renderedHtml /= Just htmlContent then do
                -- Update the state and set the inner HTML only if it has changed
                -- otherwise, selecting text would trigger a re-render
                H.modify_ \st -> st { renderedHtml = Just htmlContent }
                H.liftEffect $ setInnerHtml ref htmlContent
                pure unit
              else
                pure unit
            Nothing -> pure unit
        Nothing -> pure unit
