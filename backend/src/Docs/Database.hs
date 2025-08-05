module Docs.Database
    ( HasCheckPermission (..)
    , HasIsGroupAdmin (..)
    , HasIsSuperAdmin (..)
    , HasExistsDocument (..)
    , HasExistsTextElement (..)
    , HasExistsTextRevision (..)
    , HasExistsTreeRevision (..)
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
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)

import UserManagement.DocumentPermission (Permission)
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

import Docs.Document (Document, DocumentID)
import Docs.DocumentHistory (DocumentHistory)
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementKind
    , TextElementRef
    )
import Docs.TextRevision
    ( TextElementRevision
    , TextRevision
    , TextRevisionHistory
    , TextRevisionID
    , TextRevisionRef
    )
import Docs.Tree (Node)
import Docs.TreeRevision (TreeRevision, TreeRevisionHistory, TreeRevisionRef)
import GHC.Int (Int32)

class (HasIsSuperAdmin m) => HasCheckPermission m where
    checkDocumentPermission :: UserID -> DocumentID -> Permission -> m Bool

class (HasIsSuperAdmin m) => HasIsGroupAdmin m where
    isGroupAdmin :: UserID -> GroupID -> m Bool

class (Monad m) => HasIsSuperAdmin m where
    isSuperAdmin :: UserID -> m Bool

-- exists

class (Monad m) => HasExistsDocument m where
    existsDocument :: DocumentID -> m Bool

class (HasExistsDocument m) => HasExistsTextElement m where
    existsTextElement :: TextElementRef -> m Bool

class (HasExistsTextElement m) => HasExistsTextRevision m where
    existsTextRevision :: TextRevisionRef -> m Bool

class (HasExistsDocument m) => HasExistsTreeRevision m where
    existsTreeRevision :: TreeRevisionRef -> m Bool

-- get

class (HasCheckPermission m, HasIsGroupAdmin m, HasIsSuperAdmin m) => HasGetDocument m where
    getDocument :: DocumentID -> m (Maybe Document)
    getDocuments :: UserID -> m (Vector Document)
    getDocumentsBy :: Maybe UserID -> Maybe GroupID -> m (Vector Document)

class (HasCheckPermission m, HasExistsTreeRevision m) => HasGetTreeRevision m where
    getTreeRevision :: TreeRevisionRef -> m (TreeRevision TextElement)

class
    (HasCheckPermission m, HasExistsTextRevision m) =>
    HasGetTextElementRevision m
    where
    getTextElementRevision :: TextRevisionRef -> m (Maybe TextElementRevision)

class (HasCheckPermission m, HasExistsTextElement m) => HasGetTextHistory m where
    getTextHistory
        :: TextElementRef -> Maybe UTCTime -> Int32 -> m TextRevisionHistory

class (HasCheckPermission m, HasExistsDocument m) => HasGetTreeHistory m where
    getTreeHistory :: DocumentID -> Maybe UTCTime -> Int32 -> m TreeRevisionHistory

class (HasCheckPermission m, HasExistsDocument m) => HasGetDocumentHistory m where
    getDocumentHistory :: DocumentID -> Maybe UTCTime -> Int32 -> m DocumentHistory

-- create

class (HasIsGroupAdmin m) => HasCreateDocument m where
    createDocument :: Text -> GroupID -> UserID -> m Document

class (HasCheckPermission m, HasExistsDocument m) => HasCreateTextElement m where
    createTextElement :: DocumentID -> TextElementKind -> m TextElement

class (HasCheckPermission m, HasExistsTextElement m) => HasCreateTextRevision m where
    createTextRevision :: UserID -> TextElementRef -> Text -> m TextRevision
    getLatestTextRevisionID :: TextElementRef -> m (Maybe TextRevisionID)

class (HasCheckPermission m, HasExistsDocument m) => HasCreateTreeRevision m where
    createTreeRevision
        :: UserID
        -> DocumentID
        -> Node TextElementID
        -> m (TreeRevision TextElementID)
    existsTextElementInDocument :: DocumentID -> m (TextElementID -> Bool)
