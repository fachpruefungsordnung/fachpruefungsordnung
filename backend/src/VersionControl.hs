module VersionControl
    ( Context (..)
    , createCommit
    , getCommit
    , getDocument
    , createDocument
    , createDocumentCommit
    ) where

import Control.Arrow (left)
import Data.Functor ((<&>))
import Data.Text (Text)
import Hasql.Connection (Connection)
import Hasql.Session (Session, SessionError)
import qualified Hasql.Session as Session
import UserManagement.Group (GroupID)
import UserManagement.Group (GroupID)
import VersionControl.Commit (CommitID, CreateCommit, ExistingCommit)
import VersionControl.Document (Document, DocumentID)
import VersionControl.Error
    ( VersionControlError (..)
    , flattenVersionControlError
    )
import VersionControl.Document (Document, DocumentID)
import VersionControl.Error
    ( VersionControlError (..)
    , flattenVersionControlError
    )
import qualified VersionControl.Sessions as Sessions

-- | context for the version control (currently just a database connection)
newtype Context = Context
    { unConnection :: Connection
    }

-- | get a document by id
getDocument :: DocumentID -> Context -> IO (Either VersionControlError Document)
getDocument = runSession . Sessions.getDocument

-- | creates a new document in the specified user group
createDocument
    :: Text
    -- ^ The name of the document
    -> GroupID
    -- ^ The group which owns the document
    -> Context
    -> IO (Either VersionControlError DocumentID)
createDocument = (runSession .) . Sessions.createDocument

-- | creates a commit in the given version control context
createCommit
    :: CreateCommit
    -- ^ not-yet existing commit, intendet to be made persistent by this operation
    -> Context
    -- ^ version control context
    -> IO (Either VersionControlError ExistingCommit)
    -- ^ holds the newly created commit or an error
createCommit = runSession . Sessions.createCommit

-- | creates a commit in the specified document
--   the newly created commit must be related to the current head commit of the
--   document (if any).
createDocumentCommit
    :: DocumentID
    -> CreateCommit
    -> Context
    -> IO (Either VersionControlError Document)
createDocumentCommit documentID commit ctx =
    flattenVersionControlError
        <$> createDocumentCommit' documentID commit ctx
  where
    createDocumentCommit' = (runSession .) . Sessions.createDocumentCommit

-- | gets a commit from the given version control context
getCommit
    :: CommitID
    -- ^ the id of the commit to obtain from the database
    -> Context
    -- ^ version control context
    -> IO (Either VersionControlError ExistingCommit)
    -- ^ holds the requested commit or an error
getCommit = runSession . Sessions.getCommit

-- | runs a session and maps a potential error to a `VersionControlError`
runSession :: Session a -> Context -> IO (Either VersionControlError a)
runSession session ctx =
    Session.run
        session
        (unConnection ctx)
        <&> mapResult

-- | maps a potential error to a `VersionControlError`
mapResult :: Either SessionError a -> Either VersionControlError a
mapResult = left (DatabaseError . show)
