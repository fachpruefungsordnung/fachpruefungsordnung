module DocumentManagement.Sessions
    ( createCommit
    , getCommit
    , getVersion
    , createDocument
    , getDocument
    , createDocumentCommit
    , getCommitGraph
    )
where

import Data.Maybe (fromMaybe)
import Data.Text
import Data.Vector (Vector, toList)
import qualified Data.Vector as Vector
import DocumentManagement.Commit
import DocumentManagement.Document (Document (..), DocumentID)
import DocumentManagement.Error (DocumentError)
import DocumentManagement.Hash
import qualified DocumentManagement.Statements as Statements
import qualified DocumentManagement.Transactions as Transactions
import DocumentManagement.Tree
import Hasql.Session (Session, statement)
import Hasql.Transaction.Sessions
    ( IsolationLevel (..)
    , Mode (..)
    , transaction
    )
import UserManagement.Group (GroupID)

-- | session to obtain the whole commit graph (all commits) of a document
getCommitGraph :: DocumentID -> Session (Vector ExistingCommit)
getCommitGraph docID = do
    document <- getDocument docID
    commits <-
        mapM
            (`statement` Statements.getCommitsByRoot)
            (documentHead document)

    Vector.mapM withParents $ fromMaybe Vector.empty commits
  where
    withParents
        :: (CommitID, [CommitID] -> ExistingCommit)
        -> Session ExistingCommit
    withParents (commit, commitCons) = do
        commitParentIDs <- statement commit Statements.getCommitParentIDs
        return $ commitCons $ toList commitParentIDs

-- | session to get a commit from the database by its 'CommitID'
getCommit :: CommitID -> Session ExistingCommit
getCommit commitID = do
    commit <- statement commitID Statements.getCommit
    commitParentIDs <- statement commitID Statements.getCommitParentIDs
    replaceRoot $ commit $ toList commitParentIDs
  where
    replaceRoot (ExistingCommit header (CommitBody info (Ref ref) base)) = do
        valueRoot <- getVersion ref
        return $ ExistingCommit header $ CommitBody info (Value valueRoot) base
    replaceRoot commit = return commit

-- | session to create a new commit in the database
createCommit :: CreateCommit -> Session ExistingCommit
createCommit commit =
    transaction
        Serializable
        Write
        $ Transactions.createCommit commit

-- | session to create a new document
createDocument :: Text -> GroupID -> Session Document
createDocument = curry $ flip statement Statements.createDocument

-- | session to create a new commit in a document
createDocumentCommit
    :: DocumentID -> CreateCommit -> Session (Either DocumentError Document)
createDocumentCommit document commit =
    transaction
        Serializable
        Write
        $ Transactions.createDocumentCommit document commit

-- | session to get an existing document
getDocument :: DocumentID -> Session Document
getDocument = flip statement Statements.getDocument

-- | session to get a document tree by its hash.
--   The whole tree is obtained from the database.
--   TODO: implement a variation of this session to filter on a path through the tree.
--   This is useful, when the frontend just needs to display part of the document
--   e.g., a single paragraph.
getVersion :: Hash -> Session (Tree (Hashed NodeWithRef))
getVersion hs = do
    root <- statement hs Statements.getVersion
    withChildren root
  where
    withChildren
        :: Hashed NodeWithRef
        -> Session (Tree (Hashed NodeWithRef))
    withChildren parent@(Hashed hs' _) = do
        children <- statement hs' Statements.getChildrenByParentHash
        edges <- mapM toEdge $ toList children
        return $ Tree parent edges
    toEdge
        :: (Text, Hashed NodeWithRef)
        -> Session (Edge (Hashed NodeWithRef))
    toEdge (title, node) = do
        tree <- withChildren node
        return $ Edge title $ Value tree
