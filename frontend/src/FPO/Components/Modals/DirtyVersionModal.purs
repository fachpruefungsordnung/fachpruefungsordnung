module FPO.Components.Modals.DirtyVersionModal
  ( dirtyVersionModal
  ) where

import Prelude

import Data.Maybe (Maybe)
import FPO.Translations.Labels (Labels)
import FPO.UI.HTML (addModal)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (Translator, label, translate)

-- | Modal for notifying the user of potentially lost changes.
-- |
-- | Requires:
-- |  1. a translator for the UI texts
-- |  2. an action to cancel discarding
-- |  3. an action to proceed discarding
-- |  4. the element ID
-- |  5. the version ID to change to
dirtyVersionModal
  :: forall w action
   . Translator Labels
  -> action
  -> (Int -> Maybe Int -> action)
  -> Int
  -> Maybe Int
  -> action
  -> HH.HTML w action
dirtyVersionModal
  translator
  cancelAction
  confirmAction
  elementID
  versionID 
  doNothingAction =
  addModal (translate (label :: _ "editor_confirmSwitch") translator)
    cancelAction 
    doNothingAction $
    [ HH.div
        [ HP.classes [ HB.modalBody ] ]
        [ HH.text $ translate (label :: _ "editor_dirtySwitch") translator
        ]
    , HH.div
        [ HP.classes [ HB.modalFooter ] ]
        [ HH.button
            [ HP.type_ HP.ButtonButton
            , HP.classes
                [ HB.btn, HB.btnSecondary ]
            , HP.attr (HH.AttrName "data-bs-dismiss") "modal"
            , HE.onClick (const cancelAction)
            ]
            [ HH.text $ translate (label :: _ "common_cancel") translator ]
        , HH.button
            [ HP.type_ HP.ButtonButton
            , HP.classes [ HB.btn, HB.btnDanger ]
            , HE.onClick (const $ confirmAction elementID versionID)
            ]
            [ HH.text $ translate (label :: _ "editor_changeVersion") translator ]
        ]
    ]
