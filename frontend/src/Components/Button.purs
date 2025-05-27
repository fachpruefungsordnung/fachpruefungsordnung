module FPO.Components.Button
  ( Output(..)
  , button
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (classes) as HP
import Halogen.Themes.Bootstrap5 (btn, btnPrimary) as HB

type Input = { label :: String }
data Output = Clicked
type State = { label :: String }
data Action = Receive Input | Click

button :: forall query m. H.Component query Input Output m
button =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , receive = Just <<< Receive
        }
    }
  where
  initialState :: Input -> State
  initialState input = input

  render :: State -> H.ComponentHTML Action () m
  render { label } = HH.div []
    [ HH.small_
        [ HH.text
            "Dies ist ein Button, der dessen Code demonstrieren soll, wie input und output in Halogen funktioniert: "
        ]
    , HH.button [ HE.onClick \_ -> Click, HP.classes [ HB.btn, HB.btnPrimary ] ]
        [ HH.text label ]
    ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    Receive input -> H.modify_ _ { label = input.label }
    Click -> H.raise Clicked
