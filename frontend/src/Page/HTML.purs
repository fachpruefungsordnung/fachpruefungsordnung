-- | This module contains some HTML helpers and goodies for the FPO app,
-- | useful for creating and reusing HTML components.

module FPO.Page.HTML where

import DOM.HTML.Indexed.InputType (InputType)
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

-- Creates a new column with a label and an input field.
addColumn
  :: forall w a
   . String -- ^ value 
  -> String -- ^ label
  -> String -- ^ placeholder
  -> String -- ^ icon
  -> InputType -- ^ input type
  -> (String -> a) -- ^ action (parametrized with the value)
  -> HH.HTML w a
addColumn val str placeholder bi for act =
  HH.div
    []
    [ HH.label [ HP.classes [ HB.formLabel ] ]
        [ HH.text str ]
    , HH.div [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
        [ HH.span [ HP.classes [ HB.inputGroupText ] ]
            [ HH.i [ HP.class_ (H.ClassName bi) ] [] ]
        , HH.input
            [ HP.type_ for
            , HP.classes [ HB.formControl ]
            , HP.placeholder placeholder
            , HP.value val
            , HE.onValueInput act
            ]
        ]
    ]