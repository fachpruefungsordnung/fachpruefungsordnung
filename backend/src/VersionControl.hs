module VersionControl
    ( Context (..)
    , createCommit
    , getCommit
    , getDocument
    , createDocument
    ) where

import Control.Arrow (left)
import Data.Functor ((<&>))
import Data.Text (Text)
import Hasql.Connection (Connection)
import Hasql.Session (Session, SessionError)
import qualified Hasql.Session as Session
import UserManagement.Group (GroupID)
import VersionControl.Commit (CommitID, CreateCommit, ExistingCommit)
import VersionControl.Document (Document, DocumentID)
import VersionControl.Error (VersionControlError (..))
import qualified VersionControl.Sessions as Sessions

-- | context for the version control (currently just a database connection)
newtype Context = Context
    { unConnection :: Connection
    }

getDocument :: DocumentID -> Context -> IO (Either VersionControlError Document)
getDocument = runSession . Sessions.getDocument

createDocument
    :: Text
    -- ^ The name of the document
    -> GroupID
    -- ^ The group which owns the document
    -> Context
    -> IO (Either VersionControlError DocumentID)
createDocument = (runSession .) . Sessions.createDocument

-- FRAGE: wann wir der neue commit head? automatisch? manuell? wenn automaitsch, was wenn konflikt?
--      IDEE für AUTO: wenn conflict, dann wird head gesetzt mit merge commit. adsfsa
--
--
--          HEAD
--         /   \
--      commit  commit
--       von     von
--      user 1  user 2

-- | creates a commit in the given version control context
createCommit
    :: CreateCommit
    -- DocumentID
    -> Context
    -> IO (Either VersionControlError ExistingCommit)
createCommit = runSession . Sessions.createCommit

-- | gets a commit from the given version control context
getCommit
    :: CommitID
    -> Context
    -> IO (Either VersionControlError ExistingCommit)
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
