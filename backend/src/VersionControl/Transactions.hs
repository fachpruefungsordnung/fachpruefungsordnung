module VersionControl.Transactions
    ( createCommit
    )
where

import Hasql.Transaction (Transaction, statement)
import VersionControl.Commit
import VersionControl.Hash
import qualified VersionControl.Statements as Statements
import VersionControl.Tree

-- | transaction for creating a commit in the database in the database
createCommit :: CreateCommit -> Transaction ExistingCommit
createCommit (CreateCommit info commitRoot) = do
    root <- putVersionByRef commitRoot
    let body = CommitBody info root
    header <- statement body Statements.createCommit
    return $ ExistingCommit header body

-- | transaction for putting a document node version by its ref into the database
putVersionByRef
    :: TreeRef NodeWithMaybeRef
    -> Transaction (TreeRef (Hashed NodeWithRef))
putVersionByRef (Value tree) = Value <$> putVersion tree
putVersionByRef (Ref ref) = return $ Ref ref

-- | transaction for putting a document node version into the database
putVersion :: Tree NodeWithMaybeRef -> Transaction (Tree (Hashed NodeWithRef))
putVersion (Tree self children) = do
    selfWithRef <- ensureNodeExists self
    childrenWithHash <- mapM putChild children
    let selfWithHash = hashedTree selfWithRef childrenWithHash
    putNode selfWithHash
    mapM_ putEdge $ dataEdges selfWithHash
    return selfWithHash
  where
    putChild :: Edge NodeWithMaybeRef -> Transaction (Edge (Hashed NodeWithRef))
    putChild (Edge label childRef) = case childRef of
        Value child -> Edge label . Value <$> putVersion child
        Ref ref -> return $ Edge label (Ref ref)
    putNode :: Tree (Hashed NodeWithRef) -> Transaction ()
    putNode (Tree tree _) = statement tree Statements.putVersion
    putEdge :: DataEdge -> Transaction ()
    putEdge edge = statement edge Statements.putEdge

{- | transaction which ensures the existance of a node version in the database.
If the handed node has no 'NodeID', a new node is created.
TODO: if a node with id is handed, check if it exists. If not, just ignore the
specified 'NodeID' and treat it just like a node with no id specified.
-}
ensureNodeExists :: NodeWithMaybeRef -> Transaction NodeWithRef
ensureNodeExists (NodeWithMaybeRef (Just ref) node) = return $ NodeWithRef ref node
ensureNodeExists (NodeWithMaybeRef Nothing node) = do
    ref <- statement (nodeKind node) Statements.createNode
    return $ NodeWithRef ref node
