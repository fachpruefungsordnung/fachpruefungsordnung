{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Bifunctor (second)
import Data.Profunctor (lmap, rmap)
import Data.Text
import DocManV2.Document
    ( Document (..)
    , DocumentID (..)
    , NodeID (..)
    )
import Hasql.Statement
import Hasql.TH
import UserManagement.Group (GroupID)

-- | statement to create a node of a certain kind
createNode :: Statement (Text, Maybe NodeID) NodeID
createNode =
    rmap
        NodeID
        $ lmap
            (second (unNodeID <$>))
            [singletonStatement|
                insert into doc_nodes
                    (kind, root)
                values (
                    $1 :: text,
                    $2 :: int4?
                )
                returning id :: int4
            |]

createDocument :: Statement (Text, GroupID) DocumentID
createDocument =
    rmap
        DocumentID
        [singletonStatement|
            insert into docs
                (name, group_id)
            values
                ($1 :: text, $2 :: int4)
            returning id :: int4
        |]

-- | statement to get a document by its corresponding 'DocumentID'
getDocument :: Statement DocumentID Document
getDocument =
    rmap
        ( \(document, name, groupID, rootCommit) ->
            Document
                (DocumentID document)
                name
                groupID
                (NodeID <$> rootCommit)
        )
        $ lmap
            unDocumentID
            [singletonStatement|
                select
                    id :: int4,
                    name :: text,
                    group_id :: int4,
                    head :: int4?
                from
                    docs
                where
                    id = $1 :: int4
            |]

-- | statement to put a specific version of a document tree into the database
setDocumentHead :: Statement (DocumentID, NodeID) ()
setDocumentHead =
    lmap
        ( \(DocumentID documentId, NodeID nodeId) ->
            (documentId, nodeId)
        )
        [resultlessStatement|
            update docs
            set
                root = $2 :: int4
            where
                id = $1 :: int4
        |]
