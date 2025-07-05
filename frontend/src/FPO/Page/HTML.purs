-- | This module contains some HTML helpers and goodies for the FPO app,
-- | useful for creating and reusing HTML components and pages.

module FPO.Page.HTML where

import Prelude

import DOM.HTML.Indexed as I
import DOM.HTML.Indexed.InputType (InputType)
import Data.Maybe (Maybe(..))
import Data.String (null)
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Web.UIEvent.MouseEvent (MouseEvent)

-- Creates a new column with a optional label and an input field.
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
  HH.div_ $
    ( if null str then []
      else
        [ HH.label [ HP.classes [ HB.formLabel ] ]
            [ HH.text str ]
        ]
    )
      <>
        [ HH.div [ HP.classes [ HB.inputGroup, HB.mb4 ] ]
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
   . Boolean -- ^ whether the button is enabled
  -> String -- ^ button text
  -> Maybe String -- ^ optional, prepended icon
  -> (MouseEvent -> a) -- ^ action
  -> HH.HTML w a
addButton enabled text bi act =
  HH.div [ HP.classes [ HB.inputGroup ] ]
    [ HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes [ HB.btn, HB.btnPrimary ]
        , HE.onClick act
        , HP.disabled (not enabled)
        ]
        [ case bi of
            Just icon
            -> HH.span [ HP.class_ (H.ClassName icon) ] [ HH.text $ " " <> text ]
            Nothing
            -> HH.text text
        ]
    ]

-- | Creates a button with an icon.
addSmallButton
  :: forall w a
   . Boolean -- ^ whether the button is enabled
  -> String -- ^ button text
  -> Maybe String -- ^ optional, prepended icon
  -> (MouseEvent -> a) -- ^ action
  -> HH.HTML w a
addSmallButton enabled text bi act =
  HH.div [ HP.classes [ HB.inputGroup, HB.dFlex, HB.justifyContentEnd ] ]
    [ HH.button
        [ HP.type_ HP.ButtonButton
        , HP.classes [ HB.btn, HB.btnSm, HB.btnPrimary ]
        , HE.onClick act
        , HP.disabled (not enabled)
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
addCard
  :: forall i w
   . String -- ^ title of the card
  -> Array (HH.IProp I.HTMLdiv i) -- ^ additional properties for the card container
  -> HH.HTML w i -- ^ content of the card
  -> HH.HTML w i
addCard title e content =
  HH.div e
    [ HH.div ([ HP.classes [ HB.card ] ])
        [ HH.h5 [ HP.classes [ HB.cardHeader ] ]
            [ HH.text title ]
        , content `addClass` HB.cardBody
        ]
    ]

addModal
  :: forall w i
   . String -- ^ title of the modal
  -> (MouseEvent -> i) -- ^ action for the cancel button
  -> Array (HH.HTML w i) -- ^ content of the modal
  -> HH.HTML w i
addModal title cancelAction content =
  HH.div_
    [ HH.div
        [ HP.classes
            [ HB.modal, HB.fade, HB.show ]
        , HP.id "deleteModal"
        , HP.attr (HH.AttrName "data-bs-backdrop") "static"
        , HP.attr (HH.AttrName "data-bs-keyboard") "false"
        , HP.attr (HH.AttrName "tabindex") "-1"
        , HP.attr (HH.AttrName "aria-hidden") "false"
        , HP.style "display: block;"
        ]
        [ HH.div
            [ HP.classes [ HH.ClassName "modal-dialog" ] ]
            [ HH.div
                [ HP.classes [ HH.ClassName "modal-content" ] ]
                ( [ HH.div
                      [ HP.classes [ HH.ClassName "modal-header" ] ]
                      [ HH.h1
                          [ HP.classes
                              [ HH.ClassName "modal-title", HH.ClassName "fs-5" ]
                          ]
                          [ HH.text title ]
                      , HH.button
                          [ HP.type_ HP.ButtonButton
                          , HP.classes [ HB.btnClose ]
                          , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
                          , HP.attr (HH.AttrName "aria-label") "Close"
                          , HE.onClick cancelAction
                          ]
                          []
                      ]
                  ] <> content
                )
            ]
        ]
    , HH.div
        [ HP.classes
            [ HH.ClassName "modal-backdrop"
            , HH.ClassName "show"
            ]
        ]
        []
    ]

-- | Creates a delete button with an icon.
deleteButton :: forall w a. (MouseEvent -> a) -> HH.HTML w a
deleteButton action =
  HH.button
    [ HP.classes [ HB.btn, HB.btnOutlineDanger, HB.btnSm ]
    , HE.onClick action
    ]
    [ HH.i [ HP.class_ $ HH.ClassName "bi-trash" ] [] ]

-- | Creates an empty entry for text-based lists.
-- | Used for padding.
emptyEntryText :: forall w a. HH.HTML w a
emptyEntryText = emptyEntryGen [ HH.text "(no entry)" ]

-- | Creates an empty entry for lists with arbitrary content.
-- | Used for padding.
emptyEntryGen
  :: forall w a
   . Array (HH.HTML w a)
  -> HH.HTML w a
emptyEntryGen content =
  HH.li [ HP.classes [ HB.listGroupItem ] ]
    [ HH.div [ HP.classes [ HB.textCenter, HB.invisible ] ]
        content
    ]

-- | Adds an error message to the page.
addError
  :: forall w i
   . Maybe String -- ^ error message
  -> HH.HTML w i
addError msg =
  HH.div [ HP.classes [ HB.textCenter ] ]
    [ case msg of
        Just err -> HH.div
          [ HP.classes [ HB.alert, HB.alertDanger, HB.mt5 ] ]
          [ HH.text err ]
        Nothing -> HH.text ""
    ]
