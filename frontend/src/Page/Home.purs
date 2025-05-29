-- | Home page of the application. As of now, this is simply our
-- | "sandbox" for testing components.

module FPO.Page.Home (component) where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

data Action

type State = {}

component
  :: forall query input output m
   . MonadAff m
  => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> {}
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  render :: State -> H.ComponentHTML Action () m
  render _ =
    HH.div
      [ HP.classes
          [ HB.dFlex
          , HB.flexColumn
          , HB.justifyContentCenter
          , HB.alignItemsCenter
          , HB.h100
          , HB.bgLight
          ]
      ]
      [ HH.h1 [ HP.classes [ HB.textPrimary, HB.mb3 ] ]
          [ HH.text "Hinweis" ]
      , HH.p [ HP.classes [ HB.textCenter, HB.fs5 ] ]
          [ HH.text "Der Editor wurde zum Page Editor verlegt." ]
      ]
