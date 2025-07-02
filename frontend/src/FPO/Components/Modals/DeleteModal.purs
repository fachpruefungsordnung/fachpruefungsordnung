module FPO.Components.Modals.DeleteModal
  ( deleteConfirmationModal
  ) where

import Prelude

-- | Copied over. Redundant imports to be removed later
import FPO.Page.HTML (addModal)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap5 as HB

-- Modal for confirming the deletion of a group.
--
-- TODO: This only statically shows the modal whenever the user requests
--       to delete a group. Because of this binary show/hide logic,
--       we can't use fancy features like modal animations (fade-in, etc.).
--       Instead, we could use JSS to toggle the modal visibility, but this
--       would of course require external JavaScript code.
--         See https://getbootstrap.com/docs/5.3/components/modal/.

-- requires:
-- 1. something to determine what object to delete, like an ID
-- 2. something to derive a label for the object to delete
-- 3. an action to cancel the deletion
-- 4. an action to proceed the deletion
-- 5. a name for the type of the given object

deleteConfirmationModal
  :: forall w a action
   . a
  -> (a -> String)
  -> action
  -> (a -> action)
  -> String
  -> HH.HTML w action
deleteConfirmationModal
  objectIdentifier
  toObjectName
  cancelAction
  confirmAction
  objectTypeName =
  addModal "Confirm Delete" (const cancelAction) $
    [ HH.div
        [ HP.classes [ HB.modalBody ] ]
        [ HH.text
            ( "Are you sure you want to delete " <> objectTypeName <> " "
                <> toObjectName objectIdentifier
                <>
                  "?"
            )
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
            [ HH.text "Cancel" ]
        , HH.button
            [ HP.type_ HP.ButtonButton
            , HP.classes [ HB.btn, HB.btnDanger ]
            , HE.onClick (const $ confirmAction objectIdentifier)
            ]
            [ HH.text "Delete" ]
        ]
    ]
