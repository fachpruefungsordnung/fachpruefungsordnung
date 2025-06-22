module VersionControl.Transactions
    ( createCommit
    , createDocumentCommit
    , setDocumentHead
    )
where

import Data.Maybe (fromMaybe)
import Hasql.Transaction (Transaction, statement)
import VersionControl.Commit
import VersionControl.Document (Document (..), DocumentID, withNewDocumentHead)
import VersionControl.Error (DocumentError (..))
import VersionControl.Hash
import qualified VersionControl.Statements as Statements
import VersionControl.Tree

-- | transaction to create a new document
createDocumentCommit
    :: DocumentID -> CreateCommit -> Transaction (Either DocumentError Document)
createDocumentCommit documentId commamit = do
    commit <- createCommit commamit
    let commitId = commitHeaderID $ existingCommitHeader commit
    setDocumentHead documentId commitId

-- | transaction to update the head commit of a document
setDocumentHead
    :: DocumentID -> CommitID -> Transaction (Either DocumentError Document)
setDocumentHead documentId commitId = do
    document <- statement documentId Statements.getDocument
    oldHead <-
        mapM (`statement` Statements.getCommitNode) (documentHead document)
    case oldHead of
        Just x -> do
            newHead <- statement commitId Statements.getCommitNode
            let equalRoots =
                    fromMaybe False $
                        (==)
                            <$> commitNodeRootCommit x
                            <*> commitNodeRootCommit newHead
            if equalRoots
                then updateHead document
                else return $ Left DocumentNewHeadCommitUnrelated
        Nothing -> updateHead document
  where
    updateHead doc = do
        statement (documentId, commitId) Statements.setDocumentHead
        return $ Right (withNewDocumentHead doc commitId)

-- | transaction for creating a commit in the database
createCommit :: CreateCommit -> Transaction ExistingCommit
createCommit (CreateCommit info commitRoot) = do
    root <- putVersionByRef commitRoot
    base <- getLCACommitID $ commitInfoParents info
    let body = CommitBody info root base
    header <- statement body Statements.createCommit
    mapM_ putRel $ commitRels $ commitHeaderID header
    return $ ExistingCommit header body
  where
    commitRels self = flip CommitRel self <$> commitInfoParents info
    putRel rel = statement rel Statements.putCommitRel

-- | finds the lowest common anchestor of given commits if it exists.
--   TODO: maybe not the best implementation, but it will work for now.
getLCACommitID :: [CommitID] -> Transaction (Maybe CommitID)
getLCACommitID ids = do
    commitNodes <- mapM (`statement` Statements.getCommitNode) ids
    maybeBase <- commonBase commitNodes
    return $ commitNodeID <$> maybeBase
  where
    commonBase :: [CommitNode] -> Transaction (Maybe CommitNode)
    commonBase [] = return Nothing
    commonBase [x] = return $ Just x
    commonBase (x : y : xs) = do
        base <- commonBase2 x y
        maybe (return Nothing) (commonBase . (: xs)) base
    commonBase2 :: CommitNode -> CommitNode -> Transaction (Maybe CommitNode)
    commonBase2 x y
        | commitNodeID x == commitNodeID y = return $ Just x
        | commitNodeHeight x < commitNodeHeight y = commonBase2 y x
        | otherwise = case commitNodeBase x of
            Just baseID -> do
                base <- statement baseID Statements.getCommitNode
                commonBase2 y base
            Nothing -> return Nothing

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

-- | transaction which ensures the existance of a node version in the database.
--   If the handed node has no 'NodeID', a new node is created.
--   TODO: if a node with id is handed, check if it exists. If not, just ignore the
--   specified 'NodeID' and treat it just like a node with no id specified.
ensureNodeExists :: NodeWithMaybeRef -> Transaction NodeWithRef
ensureNodeExists (NodeWithMaybeRef (Just ref) node) = return $ NodeWithRef ref node
ensureNodeExists (NodeWithMaybeRef Nothing node) = do
    ref <- statement (nodeKind node) Statements.createNode
    return $ NodeWithRef ref node
