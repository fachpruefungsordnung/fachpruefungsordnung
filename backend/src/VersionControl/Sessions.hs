module VersionControl.Sessions
    ( createCommit
    , getCommit
    , getVersion
    )
where

import Data.Text
import Data.Vector (toList)
import Hasql.Session (Session, statement)
import Hasql.Transaction.Sessions
    ( IsolationLevel (..)
    , Mode (..)
    , transaction
    )
import VersionControl.Commit
import VersionControl.Hash
import qualified VersionControl.Statements as Statements
import qualified VersionControl.Transactions as Transactions
import VersionControl.Tree

-- | session to get a commit from the database by its 'CommitID'
getCommit :: CommitID -> Session ExistingCommit
getCommit commitID = do
    commit <- statement commitID Statements.getCommit
    replaceRoot commit
  where
    replaceRoot (ExistingCommit header (CommitBody info (Ref ref))) = do
        valueRoot <- getVersion ref
        return $ ExistingCommit header $ CommitBody info $ Value valueRoot
    replaceRoot commit = return commit

-- | session to create a new commit in the database
createCommit :: CreateCommit -> Session ExistingCommit
createCommit commit =
    transaction
        Serializable
        Write
        $ Transactions.createCommit commit

{- | session to get a document tree by its hash.
The whole tree is obtained from the database.
TODO: implement a variation of this session to filter on a path through the tree.
This is useful, when the frontend just needs to display part of the document
e.g., a single paragraph.
-}
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
