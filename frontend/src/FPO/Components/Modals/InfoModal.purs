module FPO.Components.Modals.InfoModal
  ( infoModal
  ) where

import Prelude

import Data.Array (singleton)
import FPO.Translations.Labels (Labels)
import FPO.UI.HTML (addModal)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB
import Simple.I18n.Translator (Translator, label, translate)

-- | Modal for confirming the deletion of a group.
-- |
-- | TODO: This only statically shows the modal whenever the user requests
-- |       to delete a group. Because of this binary show/hide logic,
-- |       we can't use fancy features like modal animations (fade-in, etc.).
-- |       Instead, we could use JSS to toggle the modal visibility, but this
-- |       would of course require external JavaScript code.
-- |         See https://getbootstrap.com/docs/5.3/components/modal/.
-- |
-- | Requires:
-- |  1. a translator for the UI texts
-- |  2. something to determine what object to delete, like an ID
-- |  3. something to derive a label for the object to delete
-- |  4. an action to cancel the deletion
-- |  5. an action to proceed the deletion
-- |  6. a name for the type of the given object
infoModal
  :: forall w a action
   . Translator Labels
  -> a
  -> (a -> String)
  -> action
  -> (a -> action)
  -> String
  -> HH.HTML w action
infoModal
  translator
  objectIdentifier
  toObjectName
  cancelAction
  confirmAction
  objectTypeName =
  addModal (translate (label :: _ "common_mergingInfo") translator)
    (const cancelAction) $
    [ HH.div
        [ HP.classes [ HB.modalBody ], HP.style "white-space: pre-line;" ]
        [ HH.text $
            translate (label :: _ "common_mergingInfoText") translator
        ]
    ]