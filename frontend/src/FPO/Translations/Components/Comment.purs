module FPO.Translations.Components.Comment
  ( deComment
  , enComment
  ) where

import Record.Extra (type (:::), SNil)
import Simple.I18n.Translation (Translation, fromRecord)

type EditorLabels =
  ( "comment_allComments"
      ::: "comment_comment"
      ::: "comment_delete"
      ::: "comment_delete_phrase"
      ::: "comment_discard"
      ::: "comment_discard_phrase"
      ::: "comment_end_of_conversation"
      ::: "comment_modal_delete_titel"
      ::: "comment_modal_resolve_titel"
      ::: "comment_no_timestamp"
      ::: "comment_placeholder"
      ::: "comment_predated"
      ::: "comment_reanchor"
      ::: "comment_resolve"
      ::: "comment_resolve_phrase"
      ::: "comment_resolved_notice"
      ::: "comment_send"
      ::: SNil
  )

enComment :: Translation EditorLabels
enComment = fromRecord
  { comment_allComments: "All Conversations"
  , comment_comment: "Conversation"
  , comment_delete: "Delete"
  , comment_delete_phrase: "Are you sure you want to delete this comment?"
  , comment_discard: "Discard"
  , comment_discard_phrase: "Discard this draft?"
  , comment_end_of_conversation: "End of conversation"
  , comment_modal_delete_titel: "Confirm Delete"
  , comment_modal_resolve_titel: "Confirm Resolve"
  , comment_no_timestamp: "No timestamp found."
  , comment_placeholder: "Write a comment\x2026"
  , comment_predated:
      "Note: This comment was created after the currently displayed version."
  , comment_reanchor:
      "Note: This comment no longer has an anchor. Select a new range and click the \x201CComment\x201D button to re-anchor it."
  , comment_resolve: "Resolve"
  , comment_resolve_phrase: "Are you sure you want to resolve this comment?"
  , comment_resolved_notice: "This conversation has been resolved."
  , comment_send: "Send"
  }

deComment :: Translation EditorLabels
deComment = fromRecord
  { comment_allComments: "Alle Konversationen"
  , comment_comment: "Konversation"
  , comment_delete: "Löschen"
  , comment_delete_phrase:
      "Sind sie sicher, dass sie diesen Kommentar löschen möchten?"
  , comment_discard: "Verwerfen"
  , comment_discard_phrase: "Entwurf verwerfen?"
  , comment_end_of_conversation: "Ende der Konversation"
  , comment_modal_delete_titel: "Löschen bestätigen"
  , comment_modal_resolve_titel: "Auflösen bestätigen"
  , comment_no_timestamp: "Keinen timestamp gefunden."
  , comment_placeholder: "Kommentar schreiben\x2026"
  , comment_predated:
      "Hinweis: Dieser Kommentar wurde erst nach der aktuell angezeigten Version erstellt."
  , comment_reanchor:
      "Hinweis: Dieser Kommentar hat keinen Anker mehr. Markiere neuen Bereich und klicke auf \x201EKommentar\x201C-Button, um ihn neu zu verankern."
  , comment_resolve: "Auflösen"
  , comment_resolve_phrase:
      "Sind sie sicher, dass sie diesen Kommentar auflösen möchten?"
  , comment_resolved_notice: "Diese Konversation wurde aufgelöst."
  , comment_send: "Senden"
  }
