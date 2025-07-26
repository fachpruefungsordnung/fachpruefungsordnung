module Docs.Client (Client (..)) where

import Data.Text (Text)
import Data.Time (UTCTime)

import Docs.Document (Document, DocumentID)
import Docs.DocumentHistory (DocumentHistory)
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementKind
    , TextElementRef
    )
import Docs.TextRevision
    ( NewTextRevision
    , TextElementRevision
    , TextRevision
    , TextRevisionConflict
    , TextRevisionHistory
    , TextRevisionRef
    )
import Docs.Tree (Node)
import Docs.TreeRevision
    ( TreeRevision
    , TreeRevisionHistory
    , TreeRevisionRef
    )
import Docs.Util (UserID)
import UserManagement.Group (GroupID)

-- | A document management client.
-- | Note: This client does not check for access rights as it is the responsibility of
-- | the user management module.
data Client m e = Client
    { createDocument :: Text -> GroupID -> m (Either e Document)
    , getDocument :: DocumentID -> m (Either e (Maybe Document))
    , createTextElement :: DocumentID -> TextElementKind -> m (Either e TextElement)
    , createTextRevision
        :: UserID
        -> NewTextRevision
        -> m (Either e (Either TextRevisionConflict TextRevision))
    , getTextElementRevision
        :: TextRevisionRef
        -> m (Either e (Maybe TextElementRevision))
    , createTreeRevision
        :: UserID
        -> DocumentID
        -> Node TextElementID
        -> m (Either e (TreeRevision TextElementID))
    , getTreeRevision
        :: TreeRevisionRef
        -> m (Either e (TreeRevision TextElement))
    , getTextHistory
        :: TextElementRef
        -> Maybe UTCTime
        -> m (Either e TextRevisionHistory)
    , getTreeHistory
        :: DocumentID
        -> Maybe UTCTime
        -> m (Either e TreeRevisionHistory)
    , getDocumentHistory
        :: DocumentID
        -> Maybe UTCTime
        -> m (Either e DocumentHistory)
    }
