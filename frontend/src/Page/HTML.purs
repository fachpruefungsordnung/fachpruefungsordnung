-- | This module contains some HTML helpers and goodies for the FPO app,
-- | useful for creating and reusing HTML components.

module FPO.Page.HTML where

import Prelude

import DOM.HTML.Indexed as I
import DOM.HTML.Indexed.InputType (InputType)
import Data.Maybe (Maybe(..))
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Web.UIEvent.MouseEvent (MouseEvent)

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
  HH.div_
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

-- | Creates a button with an icon.
addButton
  :: forall w a
   . String -- ^ button text
  -> Maybe String -- ^ optional, prepended icon
  -> (MouseEvent -> a) -- ^ action
  -> HH.HTML w a
addButton text bi act =
  HH.div [ HP.classes [ HB.inputGroup ] ]
    [ HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes [ HB.btn, HB.btnPrimary ]
        , HE.onClick act
        ]
        [ case bi of
            Just icon
            -> HH.span [ HP.class_ (H.ClassName icon) ] [ HH.text $ " " <> text ]
            Nothing
            -> HH.text text
        ]
    ]

-- | Wraps the given HTML in a div with the specified class.
addClass
  :: forall w i
   . H.HTML w i
  -> H.ClassName
  -> H.HTML w i
addClass html c = HH.div [ HP.classes [ c ] ] [ html ]

-- | Creates a bootstrap card with a title and content.
createCard
  :: forall i w
   . String -- ^ title of the card
  -> Array (HH.IProp I.HTMLdiv i) -- ^ additional properties for the card container
  -> HH.HTML w i -- ^ content of the card
  -> HH.HTML w i
createCard title e content =
  HH.div e
    [ HH.div [ HP.classes [ HB.card ] ]
        [ HH.h5 [ HP.classes [ HB.cardHeader ] ]
            [ HH.text title ]
        , content `addClass` HB.cardBody
        ]
    ]
