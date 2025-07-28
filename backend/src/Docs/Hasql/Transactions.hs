{-# LANGUAGE TupleSections #-}

module Docs.Hasql.Transactions
    ( createTextRevision
    , putTree
    , createTreeRevision
    , existsDocument
    , existsTextElement
    , getLatestTextRevisionID
    , isTextElementInDocument
    , hasPermission
    , isGroupAdmin
    ) where

import qualified Crypto.Hash.SHA1 as SHA1
import Hasql.Transaction (Transaction, statement)

import Data.Functor ((<&>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as Vector

import UserManagement.DocumentPermission (Permission)
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

import Docs.Document (DocumentID)
import qualified Docs.Hasql.Statements as Statements
import Docs.Hasql.TreeEdge (TreeEdge (TreeEdge), TreeEdgeChildRef (..))
import qualified Docs.Hasql.TreeEdge as TreeEdge
import Docs.TextElement (TextElementID, TextElementRef (..))
import Docs.TextRevision
    ( TextRevision
    , TextRevisionID
    )
import Docs.Tree (Edge (Edge), Node (Node), Tree (Leaf, Tree))
import Docs.TreeRevision (TreeRevision)
import DocumentManagement.Hash
    ( Hash (Hash)
    , Hashable (..)
    )

existsDocument :: DocumentID -> Transaction Bool
existsDocument = flip statement Statements.existsDocument

existsTextElement :: TextElementRef -> Transaction Bool
existsTextElement = flip statement Statements.existsTextElement

getLatestTextRevisionID :: TextElementRef -> Transaction (Maybe TextRevisionID)
getLatestTextRevisionID = (`statement` Statements.getLatestTextRevisionID)

createTextRevision
    :: UserID
    -> TextElementRef
    -> Text
    -> Transaction TextRevision
createTextRevision userID (TextElementRef _ textID) content =
    statement (textID, userID, content) Statements.createTextRevision

isTextElementInDocument :: DocumentID -> Transaction (TextElementID -> Bool)
isTextElementInDocument docID =
    statement docID Statements.getTextElementIDsForDocument
        <&> flip Set.member . Set.fromList . Vector.toList

createTreeRevision
    :: UserID
    -> DocumentID
    -> Node TextElementID
    -> Transaction (TreeRevision TextElementID)
createTreeRevision authorID docID rootNode = do
    rootHash <- putTree rootNode
    revision <- statement (docID, authorID, rootHash) Statements.putTreeRevision
    return $ revision rootNode

putTree :: Node TextElementID -> Transaction Hash
putTree (Node metaData children) = do
    childRefs <- mapM putChild children
    let ownHash =
            Hash $
                SHA1.finalize $
                    foldr
                        (flip updateHash . fst)
                        (updateHash SHA1.init metaData)
                        childRefs
    statement (ownHash, metaData) Statements.putTreeNode
    let toEdge (ref, label) idx =
            TreeEdge
                { TreeEdge.parentHash = ownHash
                , TreeEdge.position = idx
                , TreeEdge.title = label
                , TreeEdge.child = ref
                }
    let edges = zipWith toEdge childRefs [0 ..]
    mapM_ (`statement` Statements.putTreeEdge) edges
    return ownHash
  where
    putChild :: Edge TextElementID -> Transaction (TreeEdgeChildRef, Text)
    putChild (Edge label node) = putSubTree node <&> (,label)
    putSubTree :: Tree TextElementID -> Transaction TreeEdgeChildRef
    putSubTree (Leaf id_) = return $ TreeEdgeRefToTextElement id_
    putSubTree (Tree node) = putTree node <&> TreeEdgeRefToNode

hasPermission :: UserID -> DocumentID -> Permission -> Transaction Bool
hasPermission userID docID perms =
    statement (userID, docID, perms) Statements.hasDocPermission

isGroupAdmin :: UserID -> GroupID -> Transaction Bool
isGroupAdmin userID groupID =
    statement (userID, groupID) Statements.isGroupAdmin
