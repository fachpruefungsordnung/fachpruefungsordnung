{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module VersionControl.Statements
    ( createNode
    , getNodeKind
    , putVersion
    , putEdge
    , createCommit
    , putCommitRel
    , getCommitParentIDs
    , getCommit
    , getCommitNode
    , getChildrenByParentHash
    , getVersion
    , createDocument
    , getDocument
    )
where

import Data.Profunctor (lmap, rmap)
import Data.Text
import Data.Vector
import Hasql.Statement
import Hasql.TH
import UserManagement.Group (GroupID)
import VersionControl.Commit
import VersionControl.Document (Document (..), DocumentID (..))
import VersionControl.Hash (Hash (..), Hashed (..))
import VersionControl.Tree

-- | statement to create a node of a certain kind
createNode :: Statement Text NodeID
createNode =
    rmap
        NodeID
        [singletonStatement|
            insert into nodes
                (kind)
            values
                ($1 :: text)
            returning id :: int4
        |]

-- | statement to get a node kind by the corresponding 'NodeID'
getNodeKind :: Statement NodeID Text
getNodeKind =
    lmap
        unNodeID
        [singletonStatement|
            select
                kind :: text
            from
                nodes
            where
                id = $1 :: int4
        |]

-- | statement to put a specific version of a document tree into the database
putVersion :: Statement (Hashed NodeWithRef) ()
putVersion =
    lmap
        ( \(Hashed (Hash hash) (NodeWithRef (NodeID ref) node)) ->
            (hash, ref, nodeContent node)
        )
        [resultlessStatement|
            insert into node_versions
                (hash, node, content)
            values
                ($1 :: bytea, $2 :: int4, $3 :: text?)
            on conflict (hash) do nothing
        |]

-- | statement to obtain a specific version of a document tree by its hash
getVersion :: Statement Hash (Hashed NodeWithRef)
getVersion =
    rmap
        ( \(hash, node, kind, content) ->
            Hashed (Hash hash) (NodeWithRef (NodeID node) (Node kind content))
        )
        $ lmap
            (\(Hash bs) -> bs)
            [singletonStatement|
                select
                    hash :: bytea,
                    node :: int4,
                    kind :: text,
                    content :: text?
                from
                    node_versions
                    join nodes on node = id
                where
                    hash = $1 :: bytea
            |]

-- | statement to put an edge of document node version into the database
putEdge :: Statement DataEdge ()
putEdge =
    lmap
        ( \(DataEdge parent child position title) ->
            (unHash parent, unHash child, position, title)
        )
        [resultlessStatement|
            insert into trees
                (parent, child, child_position, child_title)
            values
                ($1 :: bytea, $2 :: bytea, $3 :: int4, $4 :: text)
        |]

-- | statement to obtain all child document node version by their parent's hash
getChildrenByParentHash :: Statement Hash (Vector (Text, Hashed NodeWithRef))
getChildrenByParentHash =
    rmap
        ( fmap
            ( \(title, hash, node, kind, content) ->
                (title, Hashed (Hash hash) (NodeWithRef (NodeID node) (Node kind content)))
            )
        )
        $ lmap
            (\(Hash bs) -> bs)
            [vectorStatement|
                select
                    child_title :: text,
                    hash :: bytea,
                    node :: int4,
                    kind :: text,
                    content :: text?
                from
                    node_versions
                    join trees on hash = child
                    join nodes on node = id
                where
                    parent = $1 :: bytea
                order by child_position asc
            |]

-- | statement to create a commit
createCommit :: Statement CommitBody CommitHeader
createCommit =
    rmap (\(newID, ts) -> CommitHeader (CommitID newID) ts) $
        lmap
            ( \(CommitBody info root base) ->
                ( commitInfoAuthor info
                , commitInfoMessage info
                , unHash $ treeRefHash root
                , unCommitID <$> base
                )
            )
            [singletonStatement|
                insert into commits
                    (author, message, root, base, height)
                values (
                    $1 :: uuid,
                    $2 :: text?,
                    $3 :: bytea,
                    $4 :: int4?,
                    coalesce((
                        select height + 1
                        from commits
                        where id = $4 :: int4?
                    ), 0)
                )
                returning
                    id :: int4, creation_ts :: timestamp
            |]

-- | register a parent-child relation between two commits
putCommitRel :: Statement CommitRel ()
putCommitRel =
    lmap
        (\(CommitRel parent child) -> (unCommitID parent, unCommitID child))
        [resultlessStatement|
            insert into
                commit_trees (parent, child)
            values
                ($1 :: int4, $2 :: int4)
        |]

-- | get the ids of all parents of a commit
getCommitParentIDs :: Statement CommitID (Vector CommitID)
getCommitParentIDs =
    rmap (fmap CommitID) $
        lmap
            unCommitID
            [vectorStatement|
                select
                    parent :: int4
                from
                    commit_trees
                where
                    child = $1 :: int4
            |]

-- | statement to get a commit by its 'CommitID'
getCommit :: Statement CommitID ([CommitID] -> ExistingCommit)
getCommit =
    rmap
        ( \(ref, ts, author, message, root, base) parents ->
            ExistingCommit
                (CommitHeader (CommitID ref) ts)
                ( CommitBody
                    (CommitInfo author message parents)
                    (Ref (Hash root))
                    (CommitID <$> base)
                )
        )
        $ lmap
            (\(CommitID i) -> i)
            [singletonStatement|
                select
                id :: int4,
                creation_ts :: timestamp,
                author :: uuid,
                message :: text?,
                root :: bytea,
                base :: int4?
                from
                commits
                where
                id = $1 :: int4
            |]

-- | get commit node by commit id.
-- contains metadata about a commits position in the commit graph.
getCommitNode :: Statement CommitID CommitNode
getCommitNode =
    rmap
        ( \(commit, base, height) ->
            CommitNode
                (CommitID commit)
                (CommitID <$> base)
                height
        )
        $ lmap
            unCommitID
            [singletonStatement|
                select
                    id :: int4,
                    base :: int4?,
                    height :: int4
                from
                    commits
                where
                    id = $1 :: int4
            |]

-- | statement to create a node of a certain kind
createDocument :: Statement (Text, GroupID) DocumentID
createDocument =
    rmap
        DocumentID
        [singletonStatement|
            insert into documents
                (name, group_id)
            values
                ($1 :: text),
                ($2 :: int4)
            returning id :: int4
        |]

-- | statement to get a document by its corresponding 'DocumentID'
getDocument :: Statement DocumentID Document
getDocument =
    rmap
        ( \(document, name, groupID, headCommit) ->
            Document
                (DocumentID document)
                name
                groupID
                (CommitID <$> headCommit)
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
                documents
            where
                id = $1 :: int4
        |]
