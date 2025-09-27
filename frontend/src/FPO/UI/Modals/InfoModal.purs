module FPO.UI.Modals.InfoModal
  ( infoModal
  ) where

import Prelude

import FPO.Translations.Labels (Labels)
import FPO.UI.HTML (addModal)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (Translator, label, translate)

-- | Modal for informing the user about outdated Versions.
-- |
-- | Requires:
-- |  1. a translator for the UI texts
-- |  2. an action to close the modal
-- |  3. a no-op action as a default do-nothing action
infoModal
  :: forall w action
   . Translator Labels
  -> action
  -> action
  -> HH.HTML w action
infoModal
  translator
  cancelAction
  doNothingAction =
  addModal (translate (label :: _ "editor_mergingInfo") translator)
    cancelAction
    doNothingAction $
    [ HH.div
        [ HP.classes [ HB.modalBody ], HP.style "white-space: pre-line;" ]
        [ HH.text $
            translate (label :: _ "editor_mergingInfoText") translator
        ]
    ]
