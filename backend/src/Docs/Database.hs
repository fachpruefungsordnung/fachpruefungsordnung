-- |
-- Module      : Docs.Database
-- Description : Database Abstraction for Document Management
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
--
-- This module provides an implementation-agnostic database abstraction.
-- An implementation for a PostreSQL Database is "Docs.Hasql.Database".
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
    -- | Rollback all database actions performed so far.
    rollback :: m ()

class (HasIsSuperAdmin m) => HasCheckPermission m where
    -- | Check if a user has a specific permission on a document.
    checkDocumentPermission :: UserID -> DocumentID -> Permission -> m Bool

class (HasIsSuperAdmin m) => HasIsGroupAdmin m where
    -- | Check whether a user is andmin in a group.
    isGroupAdmin :: UserID -> GroupID -> m Bool

class (Monad m) => HasIsSuperAdmin m where
    -- | Check whether a user is a super admin.
    isSuperAdmin :: UserID -> m Bool

class (Monad m) => HasNow m where
    -- | Get the current time.
    now :: m UTCTime

-- * Exists Queries

class (Monad m) => HasExistsDocument m where
    -- | Check whether a @Document@ exists in the database.
    existsDocument :: DocumentID -> m Bool

class (HasExistsDocument m) => HasExistsTextElement m where
    -- | Check whether a @TextElement@ exists in the database.
    existsTextElement :: TextElementRef -> m Bool

class (HasExistsTextElement m) => HasExistsTextRevision m where
    -- | Check whether a @TextRevision@ exists in the database.
    existsTextRevision :: TextRevisionRef -> m Bool

class (HasExistsDocument m) => HasExistsTreeRevision m where
    -- | Check whether a @TreeRevision@ exists in the database.
    existsTreeRevision :: TreeRevisionRef -> m Bool

class (HasExistsTextElement m) => HasExistsComment m where
    -- | Check whether a @Comment@ exists in the database.
    existsComment :: CommentRef -> m Bool

-- * Get Queries

class (HasCheckPermission m) => HasGetTextElement m where
    -- | Get a @TextElement@ from the database.
    getTextElement :: TextElementID -> m (Maybe TextElement)

class (HasCheckPermission m, HasIsGroupAdmin m, HasIsSuperAdmin m) => HasGetDocument m where
    -- | Get a @Document@ from the database by id.
    getDocument :: DocumentID -> m (Maybe Document)

    -- | Get all @Document@s from the database visible by the user.
    getDocuments :: UserID -> m (Vector Document)

    -- | Get all @Document@s from the database meeting the given constraints.
    getDocumentsBy :: Maybe UserID -> Maybe GroupID -> m (Vector Document)

class (HasCheckPermission m, HasExistsTreeRevision m) => HasGetTreeRevision m where
    -- | Get a @TreeRevision@ by its ref from the database.
    getTreeRevision :: TreeRevisionRef -> m (Maybe (TreeRevision TextElement))

class
    ( HasCheckPermission m
    , HasExistsTextRevision m
    ) =>
    HasGetTextElementRevision m
    where
    -- | Get a @TextElementRevision@ by the revisions ref from the database.
    getTextElementRevision :: TextRevisionRef -> m (Maybe TextElementRevision)

class (HasCheckPermission m, HasExistsTextElement m) => HasGetTextHistory m where
    -- | Get a chronological overview of @TextElement@ revisions within the specified
    -- time frame.
    getTextHistory
        :: TextElementRef
        -- ^ the text element
        -> Maybe UTCTime
        -- ^ time frame begin
        -> Maybe UTCTime
        -- ^ time frame end
        -> Int64
        -- ^ limit
        -> m TextRevisionHistory
        -- ^ chronological overview of revisions

class (HasCheckPermission m, HasExistsDocument m) => HasGetTreeHistory m where
    -- | Get a chronological overview of @Tree@ revisions within the specified
    -- time frame.
    --
    -- TODO: take same parameters as "Docs.Database.getTextHistory"
    getTreeHistory
        :: DocumentID
        -- ^ the document
        -> Maybe UTCTime
        -- ^ time frame end
        -> Int64
        -- ^ limit
        -> m TreeRevisionHistory
        -- ^ chronological overview of revisions

class (HasCheckPermission m, HasExistsDocument m) => HasGetDocumentHistory m where
    -- | Get a chronological overview of revisions of both kinds
    -- (@Tree@ and @TextElement@) for  a document within the specified time frame.
    --
    -- TODO: take same parameters as "Docs.Database.getTextHistory"
    getDocumentHistory
        :: DocumentID
        -- ^ the document
        -> Maybe UTCTime
        -- ^ time frame end
        -> Int64
        -- ^ limit
        -> m DocumentHistory
        -- ^ chronological overview of revisions

class
    (HasCheckPermission m, HasExistsDocument m, HasExistsTextElement m) =>
    HasGetComments m
    where
    -- | Get all @Comment@s for a @TextElement@.
    getComments :: TextElementRef -> m (Vector Comment)

class (HasIsSuperAdmin m) => HasGetLogs m where
    -- | Get all log messages within the specified time frame.
    getLogs
        :: Maybe UTCTime
        -- ^ offset
        -> Int64
        -- ^ limit
        -> m (Vector LogMessage)
        -- ^ log messages

class (HasExistsDocument m) => HasGetRevisionKey m where
    -- | Get the @RevisionKey@ for a @RevisionRef@.
    -- The @RevisionKey@ is a @RevisionRef@ with additional information on
    -- whether the referenced revision is a @TextElement@ revision or a
    -- @Tree@ revision.
    getRevisionKey :: RevisionRef -> m (Maybe RevisionKey)

-- * Create Queries

class (HasIsGroupAdmin m) => HasCreateDocument m where
    -- | Create a new @Document@ in the database.
    createDocument :: Text -> GroupID -> UserID -> m Document

class (HasCheckPermission m, HasExistsDocument m) => HasCreateTextElement m where
    -- | Create a new @TextElement@ in the database.
    createTextElement
        :: DocumentID
        -> TextElementKind
        -> TextElementType
        -> m TextElement

class
    (HasCheckPermission m, HasExistsTextElement m, HasNow m) =>
    HasCreateTextRevision m
    where
    -- | Update text and @CommentAnchor@s for a @TextRevision@.
    updateTextRevision
        :: TextRevisionID
        -- ^ id of the text revision
        -> Text
        -- ^ new text
        -> Vector CommentAnchor
        -- ^ new comment anchors
        -> m TextRevision
        -- ^ updated text revision

    -- | Create a new @TextRevision@ in the database.
    createTextRevision
        :: UserID
        -- ^ the user who should own the new revision
        -> TextElementRef
        -- ^ the text element
        -> Text
        -- ^ the content of the new revision
        -> Vector CommentAnchor
        -- ^ comment anchors for the revision
        -> m TextRevision
        -- ^ the newly created @TextRevision@

    -- | Get the id of the latest @TextElement@ revision.
    getLatestTextRevisionID :: TextElementRef -> m (Maybe TextRevisionID)

    -- | currently only intended as a temporary solution
    --
    -- TODO: Kann das weg? Ich glaub, das kann weg.
    updateLatestTitle :: TextElementID -> Text -> m ()

class (HasCheckPermission m, HasExistsDocument m) => HasCreateTreeRevision m where
    -- | Create a new @TreeRevision@ in the database.
    createTreeRevision
        :: UserID
        -- ^ the user who should own the new revision
        -> DocumentID
        -- ^ the document the revision belongs to
        -> Node TextElementID
        -- ^ the root node for the tree of the new revision
        -> m (TreeRevision TextElementID)
        -- ^ the newly created revision

    -- | Obtains a function to check whether a @TextElementID@ belongs to the
    -- document.
    existsTextElementInDocument
        :: DocumentID
        -- ^ the document to check whether a @TextElementID@ belongs to it
        -> m (TextElementID -> Bool)
        -- ^ the function to check the existence of the @TextElementID@ int the prior
        --   specified document

class (HasCheckPermission m, HasExistsComment m) => HasCreateComment m where
    -- | Creats a new @Comment@ in the database.
    createComment
        :: UserID
        -- ^ the user who intends to create the @Comment@
        -> TextElementID
        -- ^ the id of the @TextElement@ the comment should belong to
        -> Text
        -- ^ the content of the @Comment@s message
        -> m Comment
        -- ^ the newly created @Comment@

    -- | Mark a @Comment@ as resolved.
    resolveComment :: CommentID -> m ()

    -- | Create a new reply to an existing @Comment@.
    createReply
        :: UserID
        -- ^ the user who intends to create the reply
        -> CommentID
        -- ^ the id of the @Comment@ to reply to
        -> Text
        -- ^ the content of the replys message
        -> m Message
        -- ^ the newly created reply's @Message@

class
    (HasCheckPermission m, HasExistsTextElement m) =>
    HasDraftTextRevision m
    where
    -- | Create a new text revision draft in the database.
    createDraftTextRevision
        :: UserID
        -- ^ the user who should own the draft
        -> TextElementRef
        -- ^ the @TextElement@ the new draft should belong to
        -> TextRevisionID
        -- ^ the @TextRevision@ this draft builds upon
        -> Text
        -- ^ the content of the creaft in statu nascendi
        -> Vector CommentAnchor
        -- ^ comment anchors for the draft
        -> m DraftRevision
        -- ^ the newly created draft

    -- | Get a users @DraftRevision@ for a @TextElement@ if it exists.
    getDraftTextRevision :: UserID -> TextElementRef -> m (Maybe DraftRevision)

    -- | Delete a users @DraftRevision@ for a @TextElement@.
    deleteDraftTextRevision :: UserID -> TextElementRef -> m ()

class (Monad m) => HasLogMessage m where
    -- | Log a message to the database.
    logMessage
        :: (ToJSON v)
        => Severity
        -- ^ the severity of the event
        -> Maybe UserID
        -- ^ the user who caused the event
        -> Scope
        -- ^ the scope the event occured in
        -> v
        -- ^ the content of the message
        -> m LogMessage
        -- ^ the logged message
