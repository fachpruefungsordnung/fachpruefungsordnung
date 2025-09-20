module Docs.Database
    ( HasNow (..)
    , HasRollback (..)
    , HasCheckPermission (..)
    , HasIsGroupAdmin (..)
    , HasIsSuperAdmin (..)
    , HasExistsDocument (..)
    , HasExistsTextElement (..)
    , HasExistsTextRevision (..)
    , HasExistsTreeRevision (..)
    , HasGetTextElement (..)
    , HasGetDocument (..)
    , HasGetTreeRevision (..)
    , HasGetTextElementRevision (..)
    , HasGetTextHistory (..)
    , HasGetTreeHistory (..)
    , HasGetDocumentHistory (..)
    , HasCreateDocument (..)
    , HasCreateTextElement (..)
    , HasCreateTextRevision (..)
    , HasCreateTreeRevision (..)
    , HasGetComments (..)
    , HasCreateComment (..)
    , HasExistsComment (..)
    , HasGetLogs (..)
    , HasLogMessage (..)
    , HasGetRevisionKey (..)
    , HasDraftTextRevision (..)
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)

import UserManagement.DocumentPermission (Permission)
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

import Data.Aeson (ToJSON)
import Docs.Comment (Comment, CommentAnchor, CommentID, CommentRef, Message)
import Docs.Document (Document, DocumentID)
import Docs.DocumentHistory (DocumentHistory)
import Docs.Revision (RevisionKey, RevisionRef)
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementKind
    , TextElementRef
    , TextElementType
    )
import Docs.TextRevision
    ( DraftRevision
    , TextElementRevision
    , TextRevision
    , TextRevisionHistory
    , TextRevisionID
    , TextRevisionRef
    )
import Docs.Tree (Node)
import Docs.TreeRevision (TreeRevision, TreeRevisionHistory, TreeRevisionRef)
import GHC.Int (Int64)
import Logging.Logs (LogMessage, Scope, Severity)

class (Monad m) => HasRollback m where
    rollback :: m ()

class (HasIsSuperAdmin m) => HasCheckPermission m where
    checkDocumentPermission :: UserID -> DocumentID -> Permission -> m Bool

class (HasIsSuperAdmin m) => HasIsGroupAdmin m where
    isGroupAdmin :: UserID -> GroupID -> m Bool

class (Monad m) => HasIsSuperAdmin m where
    isSuperAdmin :: UserID -> m Bool

class (Monad m) => HasNow m where
    now :: m UTCTime

-- exists

class (Monad m) => HasExistsDocument m where
    existsDocument :: DocumentID -> m Bool

class (HasExistsDocument m) => HasExistsTextElement m where
    existsTextElement :: TextElementRef -> m Bool

class (HasExistsTextElement m) => HasExistsTextRevision m where
    existsTextRevision :: TextRevisionRef -> m Bool

class (HasExistsDocument m) => HasExistsTreeRevision m where
    existsTreeRevision :: TreeRevisionRef -> m Bool

class (HasExistsTextElement m) => HasExistsComment m where
    existsComment :: CommentRef -> m Bool

-- get

class (HasCheckPermission m) => HasGetTextElement m where
    getTextElement :: TextElementID -> m (Maybe TextElement)

class (HasCheckPermission m, HasIsGroupAdmin m, HasIsSuperAdmin m) => HasGetDocument m where
    getDocument :: DocumentID -> m (Maybe Document)
    getDocuments :: UserID -> m (Vector Document)
    getDocumentsBy :: Maybe UserID -> Maybe GroupID -> m (Vector Document)

class (HasCheckPermission m, HasExistsTreeRevision m) => HasGetTreeRevision m where
    getTreeRevision :: TreeRevisionRef -> m (Maybe (TreeRevision TextElement))

class
    (HasCheckPermission m, HasExistsTextRevision m) =>
    HasGetTextElementRevision m
    where
    getTextElementRevision :: TextRevisionRef -> m (Maybe TextElementRevision)

class (HasCheckPermission m, HasExistsTextElement m) => HasGetTextHistory m where
    getTextHistory
        :: TextElementRef
        -> Maybe UTCTime
        -> Maybe UTCTime
        -> Int64
        -> m TextRevisionHistory

class (HasCheckPermission m, HasExistsDocument m) => HasGetTreeHistory m where
    getTreeHistory :: DocumentID -> Maybe UTCTime -> Int64 -> m TreeRevisionHistory

class (HasCheckPermission m, HasExistsDocument m) => HasGetDocumentHistory m where
    getDocumentHistory :: DocumentID -> Maybe UTCTime -> Int64 -> m DocumentHistory

class
    (HasCheckPermission m, HasExistsDocument m, HasExistsTextElement m) =>
    HasGetComments m
    where
    getComments :: TextElementRef -> m (Vector Comment)

class (HasIsSuperAdmin m) => HasGetLogs m where
    getLogs
        :: Maybe UTCTime
        -- ^ offset
        -> Int64
        -- ^ limit
        -> m (Vector LogMessage)
        -- ^ log messages

class (HasExistsDocument m) => HasGetRevisionKey m where
    getRevisionKey :: RevisionRef -> m (Maybe RevisionKey)

-- create

class (HasIsGroupAdmin m) => HasCreateDocument m where
    createDocument :: Text -> GroupID -> UserID -> m Document

class (HasCheckPermission m, HasExistsDocument m) => HasCreateTextElement m where
    createTextElement
        :: DocumentID
        -> TextElementKind
        -> TextElementType
        -> m TextElement

class
    (HasCheckPermission m, HasExistsTextElement m, HasNow m) =>
    HasCreateTextRevision m
    where
    updateTextRevision
        :: TextRevisionID -> Text -> Vector CommentAnchor -> m TextRevision
    createTextRevision
        :: UserID -> TextElementRef -> Text -> Vector CommentAnchor -> m TextRevision
    getLatestTextRevisionID :: TextElementRef -> m (Maybe TextRevisionID)

    -- | currently only intended as a temporary solution
    updateLatestTitle :: TextElementID -> Text -> m ()

class (HasCheckPermission m, HasExistsDocument m) => HasCreateTreeRevision m where
    createTreeRevision
        :: UserID
        -> DocumentID
        -> Node TextElementID
        -> m (TreeRevision TextElementID)
    existsTextElementInDocument :: DocumentID -> m (TextElementID -> Bool)

class (HasCheckPermission m, HasExistsComment m) => HasCreateComment m where
    createComment :: UserID -> TextElementID -> Text -> m Comment
    resolveComment :: CommentID -> m ()
    createReply :: UserID -> CommentID -> Text -> m Message

class
    (HasCheckPermission m, HasExistsTextElement m) =>
    HasDraftTextRevision m
    where
    createDraftTextRevision
        :: UserID
        -> TextElementRef
        -> TextRevisionID
        -> Text
        -> Vector CommentAnchor
        -> m DraftRevision
    getDraftTextRevision :: UserID -> TextElementRef -> m (Maybe DraftRevision)
    deleteDraftTextRevision :: UserID -> TextElementRef -> m ()

class (Monad m) => HasLogMessage m where
    logMessage
        :: (ToJSON v)
        => Severity
        -> Maybe UserID
        -> Scope
        -> v
        -> m LogMessage
