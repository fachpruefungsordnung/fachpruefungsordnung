module Docs.DocumentHistory
    ( DocumentHistory (..)
    , DocumentHistoryItem (..)
    ) where

import Docs.Document (DocumentID)
import Docs.TextElement (TextElementID)
import Docs.TextRevision (TextRevisionHeader)
import Docs.TreeRevision (TreeRevisionHeader)

data DocumentHistoryItem
    = Tree TreeRevisionHeader
    | Text TextElementID TextRevisionHeader

data DocumentHistory
    = DocumentHistory
        DocumentID
        [DocumentHistoryItem]
