module FPO.Components.Modals.DiscardModal where

import Prelude

import Data.Array (singleton)
import FPO.Translations.Labels (Labels)
import FPO.UI.HTML (addModal)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (Translator, label, translate)

-- | Modal for confirming discarding the current draft.
-- |
-- | Requires:
-- |  1. a translator for the UI texts
-- |  2. an action to cancel discarding
-- |  3. an action to proceed discarding
discardModal
  :: forall w action
   . Translator Labels
  -> action
  -> action
  -> HH.HTML w action
discardModal
  translator
  cancelAction
  confirmAction =
  addModal (translate (label :: _ "common_confirmDiscard") translator)
    (const cancelAction) $
    [ HH.div
        [ HP.classes [ HB.modalBody ] ]
        [ HH.text $ translate (label :: _ "common_discardPhrase") translator
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
            , HE.onClick (const confirmAction)
            ]
            [ HH.text $ translate (label :: _ "common_discard") translator ]
        ]
    ]

