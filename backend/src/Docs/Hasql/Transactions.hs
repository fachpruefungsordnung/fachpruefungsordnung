{-# LANGUAGE TupleSections #-}

module Docs.Hasql.Transactions
    ( createTextRevision
    , putTree
    , createTreeRevision
    ) where

import qualified Crypto.Hash.SHA1 as SHA1
import Hasql.Transaction (Transaction, statement)

import Control.Monad (unless)
import Data.Functor ((<&>))
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Vector as Vector

import Docs.Document (DocumentID)
import qualified Docs.Hasql.Statements as Statements
import Docs.Hasql.TreeEdge (TreeEdge (TreeEdge), TreeEdgeChildRef (..))
import qualified Docs.Hasql.TreeEdge as TreeEdge
import Docs.TextElement (TextElementID, TextElementRef (..))
import Docs.TextRevision
    ( NewTextRevision
    , TextRevision
    , TextRevisionConflict
    , newTextRevision
    )
import Docs.Tree (Edge (..), Node (..), Tree (..))
import Docs.TreeRevision
    ( TreeRevision
    , newTreeRevision
    )
import Docs.Util (UserID)
import DocumentManagement.Hash
    ( Hash (Hash)
    , Hashable (..)
    )

createTextRevision
    :: UserID
    -- ^ the id of the user who intends to create the new revision
    -> NewTextRevision
    -- ^ all data needed to create a new text revision
    -> Transaction (Either TextRevisionConflict TextRevision)
    -- ^ either the newly created text revision or a conflict
createTextRevision =
    newTextRevision
        (`statement` Statements.getLatestTextRevisionID)
        createTextRevision'
  where
    createTextRevision' textRef@(TextElementRef _ textID) userID content = do
        exists <- statement textRef Statements.existsTextElement
        unless exists $ error "TextElement does not exist."
        statement (textID, userID, content) Statements.createTextRevision

createTreeRevision
    :: UserID
    -> DocumentID
    -> Node TextElementID
    -> Transaction (TreeRevision TextElementID)
createTreeRevision = newTreeRevision isTextElementInDocument createTreeRevision'
  where
    isTextElementInDocument :: DocumentID -> Transaction (TextElementID -> Bool)
    isTextElementInDocument docID =
        statement docID Statements.getTextElementIDsForDocument
            <&> flip Set.member . Set.fromList . Vector.toList
    createTreeRevision'
        :: UserID
        -> DocumentID
        -> Node TextElementID
        -> Transaction (TreeRevision TextElementID)
    createTreeRevision' authorID docID rootNode = do
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
