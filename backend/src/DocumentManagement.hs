module DocumentManagement
    ( Context (..)
    , createCommit
    , getCommit
    , getDocument
    , createDocument
    , createDocumentCommit
    , getCommitGraph
    ) where

import Control.Arrow (left)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Vector (Vector)
import DocumentManagement.Commit (CommitID, CreateCommit, ExistingCommit)
import DocumentManagement.Document (Document, DocumentID)
import DocumentManagement.Error
    ( DocumentManagementError (..)
    , flattenDocumentManagementError
    )
import qualified DocumentManagement.Sessions as Sessions
import Hasql.Connection (Connection)
import Hasql.Session (Session, SessionError)
import qualified Hasql.Session as Session
import UserManagement.Group (GroupID)

-- | context for the version control (currently just a database connection)
newtype Context = Context
    { unConnection :: Connection
    }

-- | obtains all commits belonging to the specified document from the database.
--   the result is a vector of commits. since commits contain all information
--   about their relation to other commits, this list alone defines the commit graph.
--
--   TODO: currently, this fetches the WHOLE commit graph, meaning ALL commits of
--   a document together with ALL relations between commits.
--   For documents with a large history, this might become expensive.
--   Possible solutions:
--      - take timestamp and omit all commits older than this timestamp
--      - limit number of commits returned. this could be done with ORDER BY and LIMIT.
getCommitGraph
    :: DocumentID
    -- ^ the id of the document of the requested commits graph
    -> Context
    -- ^ version control context
    -> IO (Either DocumentManagementError (Vector ExistingCommit))
getCommitGraph = runSession . Sessions.getCommitGraph

-- | get a document by id
getDocument
    :: DocumentID
    -- ^ the id of the document to get
    -> Context
    -- ^ version control context
    -> IO (Either DocumentManagementError Document)
    -- ^ holds the requested document or an error
getDocument = runSession . Sessions.getDocument

-- | creates a new document in the specified user group
createDocument
    :: Text
    -- ^ The name of the document
    -> GroupID
    -- ^ The group which owns the document
    -> Context
    -- ^ version control context
    -> IO (Either DocumentManagementError Document)
    -- ^ holds the newly created document or an error
createDocument = (runSession .) . Sessions.createDocument

-- | creates a commit in the given version control context
createCommit
    :: CreateCommit
    -- ^ not-yet existing commit, intendet to be made persistent by this operation
    -> Context
    -- ^ version control context
    -> IO (Either DocumentManagementError ExistingCommit)
    -- ^ holds the newly created commit or an error
createCommit = runSession . Sessions.createCommit

-- | creates a commit in the specified document
--   the newly created commit must be related to the current head commit of the
--   document (if any).
--
--   TODO: if the commit is created sucessfully, but can not be set as head of the
--         document, the commit will persist in the database. I think, this is
--         reasonable, as it can be used later (e.g., to merge it in).
--         However, it is currently not returned in this case, which should be
--         changed imo.
createDocumentCommit
    :: DocumentID
    -- ^ the id of the document the commit should belong to
    -> CreateCommit
    -- ^ not-yet existing commit, intendet to be made persistent by this operation
    -> Context
    -- ^ version control context
    -> IO (Either DocumentManagementError Document)
    -- ^ holds the document with the newly created commit or an error
createDocumentCommit documentID commit ctx =
    flattenDocumentManagementError
        <$> createDocumentCommit' documentID commit ctx
  where
    createDocumentCommit' = (runSession .) . Sessions.createDocumentCommit

-- | gets a commit from the given version control context
getCommit
    :: CommitID
    -- ^ the id of the commit to obtain from the database
    -> Context
    -- ^ version control context
    -> IO (Either DocumentManagementError ExistingCommit)
    -- ^ holds the requested commit or an error
getCommit = runSession . Sessions.getCommit

-- | runs a session and maps a potential error to a `DocumentManagementError`
runSession :: Session a -> Context -> IO (Either DocumentManagementError a)
runSession session ctx =
    Session.run
        session
        (unConnection ctx)
        <&> mapResult

-- | maps a potential error to a `DocumentManagementError`
mapResult :: Either SessionError a -> Either DocumentManagementError a
mapResult = left (DatabaseError . show)
