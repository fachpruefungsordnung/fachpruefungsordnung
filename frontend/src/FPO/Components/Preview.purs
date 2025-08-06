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
  { renderedHtml :: Maybe (String)
  }

type Input = { renderedHtml :: Maybe String }

preview :: forall m. MonadAff m => H.Component Query Input Output m
preview = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      , receive = Just <<< Receive
      }
  }
  where
  initialState :: State
  initialState =
    { renderedHtml: Nothing
    }

  render :: State -> H.ComponentHTML Action Slots m
  render _ =
    HH.div [ ref (RefLabel "injectHtml") ] [ HH.text "No content rendered yet." ]

  handleAction :: MonadAff m => Action -> H.HalogenM State Action Slots Unit m Unit
  handleAction = case _ of
    Initialize -> do
      pure unit

    Receive { renderedHtml } -> do
      htmlElementRef <- getHTMLElementRef (RefLabel "injectHtml")
      case htmlElementRef of
        Just ref -> do
          case renderedHtml of
            Just htmlContent -> do
              H.modify_ \st -> st { renderedHtml = Just htmlContent }
              H.liftEffect $ setInnerHtml ref htmlContent
              pure unit
            Nothing -> pure unit
        Nothing -> pure unit
