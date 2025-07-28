module Docs.Database
    ( HasCheckDocPermission (..)
    , HasIsGroupAdmin (..)
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

class (Monad m) => HasCheckDocPermission m where
    checkDocumentPermission :: UserID -> DocumentID -> Permission -> m Bool

class (Monad m) => HasIsGroupAdmin m where
    isGroupAdmin :: UserID -> GroupID -> m Bool

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

class (HasCheckDocPermission m) => HasGetDocument m where
    getDocument :: DocumentID -> m (Maybe Document)
    getDocuments :: UserID -> m (Vector Document)

class (HasCheckDocPermission m, HasExistsTreeRevision m) => HasGetTreeRevision m where
    getTreeRevision :: TreeRevisionRef -> m (TreeRevision TextElement)

class
    (HasCheckDocPermission m, HasExistsTextRevision m) =>
    HasGetTextElementRevision m
    where
    getTextElementRevision :: TextRevisionRef -> m (Maybe TextElementRevision)

class (HasCheckDocPermission m, HasExistsTextElement m) => HasGetTextHistory m where
    getTextHistory :: TextElementRef -> Maybe UTCTime -> m TextRevisionHistory

class (HasCheckDocPermission m, HasExistsDocument m) => HasGetTreeHistory m where
    getTreeHistory :: DocumentID -> Maybe UTCTime -> m TreeRevisionHistory

class (HasCheckDocPermission m, HasExistsDocument m) => HasGetDocumentHistory m where
    getDocumentHistory :: DocumentID -> Maybe UTCTime -> m DocumentHistory

-- create

class (HasIsGroupAdmin m) => HasCreateDocument m where
    createDocument :: Text -> GroupID -> m Document

class (HasCheckDocPermission m, HasExistsDocument m) => HasCreateTextElement m where
    createTextElement :: DocumentID -> TextElementKind -> m TextElement

class (HasCheckDocPermission m, HasExistsTextElement m) => HasCreateTextRevision m where
    createTextRevision :: UserID -> TextElementRef -> Text -> m TextRevision
    getLatestTextRevisionID :: TextElementRef -> m (Maybe TextRevisionID)

class (HasCheckDocPermission m, HasExistsDocument m) => HasCreateTreeRevision m where
    createTreeRevision
        :: UserID
        -> DocumentID
        -> Node TextElementID
        -> m (TreeRevision TextElementID)
    existsTextElementInDocument :: DocumentID -> m (TextElementID -> Bool)
