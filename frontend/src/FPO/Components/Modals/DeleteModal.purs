module FPO.Components.Modals.DeleteModal
  ( deleteConfirmationModal
  ) where

import Prelude

import Data.Array (singleton)
import Data.Maybe (Maybe(..))
import FPO.Translations.Labels (Labels)
import FPO.UI.HTML (addModal)
import FPO.Util as Util
import Halogen as H
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
-- |  6. a no-op action as a default do-nothing action
-- |  7. an optional reference label for the delete button
--       (e.g., for automatic focus).
-- |  8. a name for the type of the given object
-- |
-- | TODO: As of now, this modal exposes a reference label interface for the
-- |       delete button. This allows for two things:
-- |        1. Focusing the delete button when the modal opens, for fast keyboard
-- |           interaction (main use case).
-- |        2. Just focusing the modal in general, allowing for closing it with
-- |           "Escape" (secondary use case).
-- |       We might not want to have the delete button focused by default,
-- |       but rather just the modal itself. This would allow for closing the
-- |       modal with "Escape", but would require the user to tab to the delete
-- |       button before being able to confirm the deletion with "Enter". For this,
-- |       we can easily just associate the reference label with some other element,
-- |       like the modal body or footer.
deleteConfirmationModal
  :: forall w a action
   . Translator Labels
  -> a
  -> (a -> String)
  -> action
  -> (a -> action)
  -> action
  -> Maybe H.RefLabel
  -> String
  -> HH.HTML w action
deleteConfirmationModal
  translator
  objectIdentifier
  toObjectName
  cancelAction
  confirmAction
  doNothingAction
  mRefLabel
  objectTypeName =
  addModal (translate (label :: _ "common_confirmDelete") translator)
    cancelAction
    doNothingAction $
    [ HH.div
        [ HP.classes [ HB.modalBody ]
        , HE.onKeyDown $ Util.handleKeyDown
            cancelAction
            (confirmAction objectIdentifier)
            doNothingAction
        ]
        [ HH.text $
            translate (label :: _ "common_deletePhraseA") translator
              <> objectTypeName
              <> " "
        , HH.i_ $ singleton $ HH.text $
            toObjectName objectIdentifier
        , HH.text $
            translate (label :: _ "common_deletePhraseB") translator
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
            ( [ HP.type_ HP.ButtonButton
              , HP.classes [ HB.btn, HB.btnDanger ]
              , HE.onClick (const $ confirmAction objectIdentifier)
              ]
                <>
                  case mRefLabel of
                    Just refLabel -> [ HP.ref refLabel ]
                    Nothing -> []
            )
            [ HH.text $ translate (label :: _ "common_delete") translator ]
        ]
    ]
