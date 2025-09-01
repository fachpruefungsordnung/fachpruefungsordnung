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
      ::: "comment_no_timestamp"
      ::: "comment_resolve"
      ::: "comment_send"
      ::: SNil
  )

enComment :: Translation EditorLabels
enComment = fromRecord
  { comment_allComments: "All Conversations"
  , comment_comment: "Conversation"
  , comment_delete: "Delete"
  , comment_no_timestamp: "No timestamp found."
  , comment_resolve: "Resolve"
  , comment_send: "Send"
  }

deComment :: Translation EditorLabels
deComment = fromRecord
  { comment_allComments: "Alle Konversationen"
  , comment_comment: "Konversation"
  , comment_delete: "Löschen"
  , comment_no_timestamp: "Keinen timestamp gefunden."
  , comment_resolve: "Auflösen"
  , comment_send: "Senden"
  }
