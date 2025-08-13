{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Docs.Hasql.Statements
    ( now
    , createDocument
    , getDocument
    , getDocuments
    , getDocumentsBy
    , createTextElement
    , getTextElement
    , updateTextRevision
    , createTextRevision
    , getTextRevision
    , getLatestTextRevisionID
    , getTextElementRevision
    , getTreeNode
    , putTreeNode
    , putTreeEdge
    , putTreeRevision
    , getTreeRevision
    , getTreeRevisionHistory
    , getTextRevisionHistory
    , getTextElementIDsForDocument
    , getTreeEdgesByParent
    , getDocumentRevisionHistory
    , existsDocument
    , existsTreeRevision
    , existsTextElement
    , existsTextRevision
    , hasPermission
    , isGroupAdmin
    ) where

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (first), bimap)
import Data.ByteString (ByteString)
import Data.Profunctor (lmap, rmap)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Tuple.Curry (uncurryN)
import Data.UUID (UUID)
import Data.Vector (Vector, mapMaybe)
import GHC.Int (Int32)

import Hasql.Statement (Statement)
import Hasql.TH
    ( maybeStatement
    , resultlessStatement
    , singletonStatement
    , vectorStatement
    )

import UserManagement.DocumentPermission (Permission)
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

import qualified Data.Text as Text
import Docs.Document (Document (Document), DocumentID (..))
import qualified Docs.Document as Document
import Docs.DocumentHistory (DocumentHistoryItem)
import qualified Docs.DocumentHistory as DocumentHistory
import Docs.Hasql.TreeEdge
    ( TreeEdge
    , TreeEdgeChild (..)
    , TreeEdgeChildRef (..)
    )
import qualified Docs.Hasql.TreeEdge as TreeEdge
import Docs.TextElement
    ( TextElement (TextElement)
    , TextElementID (..)
    , TextElementKind
    , TextElementRef (..)
    )
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( TextElementRevision (TextElementRevision)
    , TextRevision (TextRevision)
    , TextRevisionHeader (TextRevisionHeader)
    , TextRevisionID (..)
    , TextRevisionRef (..)
    , TextRevisionSelector (..)
    , specificTextRevision
    )
import qualified Docs.TextRevision as TextRevision
import Docs.Tree (Node, NodeHeader (NodeHeader))
import qualified Docs.Tree as Tree
import Docs.TreeRevision
    ( TreeRevision (TreeRevision)
    , TreeRevisionHeader (TreeRevisionHeader)
    , TreeRevisionID (..)
    , TreeRevisionRef (..)
    , specificTreeRevision
    )
import qualified Docs.TreeRevision as TreeRevision
import Docs.UserRef (UserRef (UserRef))
import qualified Docs.UserRef as UserRef
import DocumentManagement.Hash (Hash (..))

now :: Statement () UTCTime
now =
    [singletonStatement|
        SELECT now() :: timestamptz
    |]

existsDocument :: Statement DocumentID Bool
existsDocument =
    lmap
        unDocumentID
        [singletonStatement|
            SELECT EXISTS (
                SELECT
                    1 :: bool
                FROM
                    docs
                WHERE
                    id = $1 :: int4
            ) :: bool
        |]

existsTreeRevision :: Statement TreeRevisionRef Bool
existsTreeRevision =
    lmap
        uncurryTreeRevisionRef
        [singletonStatement|
            SELECT EXISTS (
                SELECT
                    1
                FROM
                    doc_tree_revisions
                WHERE
                    document = $1 :: int4
                    AND ($2 :: int4? IS NULL OR id = $2 :: int4?)
            ) :: bool
        |]

existsTextElement :: Statement TextElementRef Bool
existsTextElement =
    lmap
        uncurryTextElementRef
        [singletonStatement|
            SELECT EXISTS (
                SELECT
                    1
                FROM
                    doc_text_elements
                WHERE
                    document = $1 :: int4
                    AND id = $2 :: int4
            ) :: bool
        |]

existsTextRevision :: Statement TextRevisionRef Bool
existsTextRevision =
    lmap
        uncurryTextRevisionRef
        [singletonStatement|
            SELECT EXISTS (
                SELECT
                    1
                FROM
                    doc_text_revisions tr
                    JOIN doc_text_elements te on tr.text_element = te.id
                WHERE
                    te.document = $1 :: int4
                    AND tr.text_element = $2 :: int4
                    AND ($3 :: int4? IS NULL OR tr.id = $3 :: int4?)
            ) :: bool
        |]

uncurryDocument
    :: ( Int32
       , Text
       , Int32
       , UTCTime
       , UUID
       , Text
       , UTCTime
       , UUID
       , Text
       )
    -> Document
uncurryDocument
    ( id_
        , name
        , groupID
        , created
        , createdByID
        , createdByName
        , lastEdited
        , lastEditedByID
        , lastEditedByName
        ) =
        Document
            { Document.identifier = DocumentID id_
            , Document.name = name
            , Document.group = groupID
            , Document.created = created
            , Document.createdBy =
                UserRef
                    { UserRef.identifier = createdByID
                    , UserRef.name = createdByName
                    }
            , Document.lastEdited = lastEdited
            , Document.lastEditedBy =
                UserRef
                    { UserRef.identifier = lastEditedByID
                    , UserRef.name = lastEditedByName
                    }
            }

createDocument :: Statement (Text, GroupID, UserID) Document
createDocument =
    rmap
        uncurryDocument
        [singletonStatement|
            WITH inserted AS (
                insert into docs
                    (name, "group", created_by)
                values
                    ($1 :: text, $2 :: int4, $3 :: uuid)
                returning
                    id :: int4,
                    name :: text,
                    "group" :: int4,
                    creation_ts :: timestamptz?,
                    created_by :: uuid?
            )
            SELECT
                inserted.id :: int4,
                inserted.name :: text,
                inserted."group" :: int4,
                inserted.creation_ts :: timestamptz,
                inserted.created_by :: uuid,
                users.name :: text,
                inserted.creation_ts :: timestamptz,
                inserted.created_by :: uuid,
                users.name :: text
            FROM
                inserted
                LEFT JOIN users ON inserted.created_by = users.id
        |]

getDocument :: Statement DocumentID (Maybe Document)
getDocument =
    lmap unDocumentID $
        rmap
            (uncurryDocument <$>)
            [maybeStatement|
                SELECT
                    d.id :: int4,
                    d.name :: text,
                    d."group" :: int4,
                    d.creation_ts :: timestamptz,
                    d.created_by :: uuid,
                    cu.name :: text,
                    COALESCE(r.creation_ts, d.creation_ts) :: timestamptz,
                    COALESCE(r.author_id, d.created_by) :: uuid,
                    COALESCE(r.author_name, cu.name) :: text
                FROM
                    docs d
                    LEFT JOIN LATERAL (
                        SELECT
                            dr.creation_ts,
                            dr.author AS author_id,
                            u.name AS author_name
                        FROM
                            doc_revisions dr
                            JOIN users u ON dr.author = u.id
                        WHERE
                            dr.document = d.id
                        ORDER BY
                            dr.creation_ts DESC
                        LIMIT 1
                    ) r ON TRUE
                    LEFT JOIN users cu ON d.created_by = cu.id
                WHERE
                    d.id = $1 :: int4
            |]

getDocuments :: Statement UserID (Vector Document)
getDocuments =
    rmap
        (uncurryDocument <$>)
        [vectorStatement|
            SELECT DISTINCT
                d.id :: int4,
                d.name :: text,
                d."group" :: int4,
                d.creation_ts :: timestamptz,
                d.created_by :: uuid,
                cu.name :: text,
                COALESCE(dr.creation_ts, d.creation_ts) :: timestamptz AS last_edited,
                COALESCE(dr.author_id, d.created_by) :: uuid,
                COALESCE(dr.author_name, cu.name) :: text
            FROM
                docs d
                LEFT JOIN roles r ON r.group_id = d."group"
                LEFT JOIN external_document_rights edr ON d.id = edr.document_id
                LEFT JOIN LATERAL (
                    SELECT
                        dr.creation_ts,
                        dr.author AS author_id,
                        u.name AS author_name
                    FROM
                        doc_revisions dr
                        JOIN users u ON dr.author = u.id
                    WHERE
                        dr.document = d.id
                    ORDER BY
                        dr.creation_ts DESC
                    LIMIT 1
                ) dr ON TRUE
                LEFT JOIN users cu ON d.created_by = cu.id
            WHERE
                r.user_id = $1 :: uuid
                OR edr.user_id = $1 :: uuid
            ORDER BY
                last_edited DESC
        |]

getDocumentsBy :: Statement (Maybe UserID, Maybe GroupID) (Vector Document)
getDocumentsBy =
    rmap
        (uncurryDocument <$>)
        [vectorStatement|
            SELECT DISTINCT
                d.id :: int4,
                d.name :: text,
                d."group" :: int4,
                d.creation_ts :: timestamptz,
                d.created_by :: uuid,
                cu.name :: text,
                COALESCE(dr.creation_ts, d.creation_ts) :: timestamptz AS last_edited,
                COALESCE(dr.author_id, d.created_by) :: uuid,
                COALESCE(dr.author_name, cu.name) :: text
            FROM
                docs d
                LEFT JOIN roles r ON r.group_id = d."group"
                LEFT JOIN external_document_rights edr ON d.id = edr.document_id
                LEFT JOIN LATERAL (
                    SELECT
                        dr.creation_ts,
                        dr.author AS author_id,
                        u.name AS author_name
                    FROM
                        doc_revisions dr
                        JOIN users u ON dr.author = u.id
                    WHERE
                        dr.document = d.id
                    ORDER BY
                        dr.creation_ts DESC
                    LIMIT 1
                ) dr ON TRUE
                LEFT JOIN users cu ON d.created_by = cu.id
            WHERE
                r.user_id = $1 :: uuid?
                OR edr.user_id = $1 :: uuid?
                OR d."group" = $2 :: int4?
            ORDER BY
                last_edited DESC
        |]

uncurryTextElement :: (Int32, Text) -> TextElement
uncurryTextElement (id_, kind) =
    TextElement
        { TextElement.identifier = TextElementID id_
        , TextElement.kind = kind
        }

createTextElement :: Statement (DocumentID, Text) TextElement
createTextElement =
    lmap mapInput $
        rmap
            uncurryTextElement
            [singletonStatement|
            insert into doc_text_elements
                (document, kind)
            values
                ($1 :: int4, $2 :: text)
            returning
                id :: int4,
                kind :: text
        |]
  where
    mapInput (docID, kind) = (unDocumentID docID, kind)

getTextElement :: Statement TextElementID (Maybe TextElement)
getTextElement =
    lmap
        unTextElementID
        $ rmap
            (uncurryTextElement <$>)
            [maybeStatement|
                select
                    id :: int4,
                    kind :: text
                from
                    doc_text_elements
                where
                    id = $1 :: int4
            |]

uncurryTextRevisionHeader :: (Int32, UTCTime, UUID, Text) -> TextRevisionHeader
uncurryTextRevisionHeader (id_, timestamp, authorID, authorName) =
    TextRevisionHeader
        { TextRevision.identifier = TextRevisionID id_
        , TextRevision.timestamp = timestamp
        , TextRevision.author =
            UserRef
                { UserRef.identifier = authorID
                , UserRef.name = authorName
                }
        }

uncurryTextRevision :: (Int32, UTCTime, UUID, Text, Text) -> TextRevision
uncurryTextRevision (id_, timestamp, authorID, authorName, content) =
    TextRevision
        (uncurryTextRevisionHeader (id_, timestamp, authorID, authorName))
        content

uncurryTextElementRevision
    :: ( Int32
       , TextElementKind
       , Maybe Int32
       , Maybe UTCTime
       , Maybe UUID
       , Maybe Text
       , Maybe Text
       )
    -> TextElementRevision
uncurryTextElementRevision
    (id_, kind, revisionID, timestamp, authorID, authorName, content) =
        TextElementRevision
            TextElement
                { TextElement.identifier = TextElementID id_
                , TextElement.kind = kind
                }
            $ do
                trRevisionID <- revisionID
                trTimestamp <- timestamp
                trAuthorID <- authorID
                trAuthorName <- authorName
                TextRevision
                    TextRevisionHeader
                        { TextRevision.identifier = TextRevisionID trRevisionID
                        , TextRevision.timestamp = trTimestamp
                        , TextRevision.author =
                            UserRef
                                { UserRef.identifier = trAuthorID
                                , UserRef.name = trAuthorName
                                }
                        }
                    <$> content

updateTextRevision :: Statement (TextRevisionID, Text) TextRevision
updateTextRevision =
    lmap
        (first unTextRevisionID)
        $ rmap
            uncurryTextRevision
            [singletonStatement|
                WITH updated AS (
                    UPDATE
                        doc_text_revisions
                    SET
                        id = nextval('doc_text_revision_seq'),
                        creation_ts = now(),
                        content = $2 :: text
                    WHERE
                        id = $1 :: int4
                    RETURNING
                        id :: int4,
                        creation_ts :: timestamptz,
                        author :: uuid,
                        content :: text
                )
                SELECT
                    updated.id :: int4,
                    updated.creation_ts :: timestamptz,
                    updated.author :: uuid,
                    users.name :: text,
                    updated.content :: text
                FROM
                    updated
                    JOIN users on users.id = updated.author
            |]

createTextRevision :: Statement (TextElementID, UUID, Text) TextRevision
createTextRevision =
    lmap
        mapInput
        $ rmap
            uncurryTextRevision
            [singletonStatement|
                WITH inserted AS (
                    insert into doc_text_revisions
                        (text_element, author, content)
                    values
                        ($1 :: int4, $2 :: uuid, $3 :: text)
                    returning
                        id :: int4,
                        creation_ts :: timestamptz,
                        author :: uuid,
                        content :: text
                )
                SELECT
                    inserted.id :: int4,
                    inserted.creation_ts :: timestamptz,
                    inserted.author :: uuid,
                    users.name :: text,
                    inserted.content :: text
                FROM
                    inserted
                    JOIN users on users.id = inserted.author
            |]
  where
    mapInput (elementID, author, content) =
        (unTextElementID elementID, author, content)

getTextRevision
    :: Statement (TextElementID, TextRevisionSelector) (Maybe TextRevision)
getTextRevision =
    lmap
        (bimap unTextElementID ((unTextRevisionID <$>) . specificTextRevision))
        $ rmap
            (uncurryTextRevision <$>)
            [maybeStatement|
                select
                    tr.id :: int4,
                    tr.creation_ts :: timestamptz,
                    tr.author :: uuid,
                    u.name :: text,
                    tr.content :: text
                from
                    doc_text_revisions tr
                    join users u on tr.author = u.id
                where
                    tr.text_element = $1 :: int4
                    and ($2 :: int4? is null or tr.id = $2 :: int4?)
                order by
                    tr.creation_ts desc
                limit 1
            |]

getTextRevisionHistory
    :: Statement (TextElementRef, Maybe UTCTime, Int32) (Vector TextRevisionHeader)
getTextRevisionHistory =
    lmap
        mapInput
        $ rmap
            (uncurryTextRevisionHeader <$>)
            [vectorStatement|
                SELECT
                    tr.id :: INT4,
                    tr.creation_ts :: TIMESTAMPTZ,
                    tr.author :: UUID,
                    u.name :: TEXT
                FROM
                    doc_text_revisions tr
                    JOIN doc_text_elements te ON tr.text_element = te.id
                    JOIN users u on tr.author = u.id
                WHERE
                    te.document = $1 :: INT4
                    AND tr.text_element = $2 :: INT4
                    AND tr.creation_ts < COALESCE($3 :: TIMESTAMPTZ?, NOW())
                ORDER BY
                    tr.creation_ts DESC
                LIMIT
                    $4 :: INT4
            |]
  where
    mapInput (TextElementRef docID textID, maybeTimestamp, limit) =
        (unDocumentID docID, unTextElementID textID, maybeTimestamp, limit)

getLatestTextRevisionID :: Statement TextElementRef (Maybe TextRevisionID)
getLatestTextRevisionID =
    lmap
        uncurryTextElementRef
        $ rmap
            (TextRevisionID <$>)
            [maybeStatement|
                select
                    tr.id :: int4
                from
                    doc_text_revisions tr
                    join doc_text_elements te on tr.text_element = te.id
                where
                    te.document = $1 :: int4
                    and tr.text_element = $2 :: int4
                order by
                    tr.creation_ts desc
                limit 1
            |]

uncurryTextElementRef :: TextElementRef -> (Int32, Int32)
uncurryTextElementRef (TextElementRef docID textID) = (unDocumentID docID, unTextElementID textID)

getTextElementRevision
    :: Statement TextRevisionRef (Maybe TextElementRevision)
getTextElementRevision =
    lmap
        uncurryTextRevisionRef
        $ rmap
            (uncurryTextElementRevision <$>)
            [maybeStatement|
                select
                    te.id :: int4,
                    te.kind :: text,
                    tr.id :: int4?,
                    tr.creation_ts :: timestamptz?,
                    tr.author :: uuid?,
                    u.name :: text?,
                    tr.content :: text?
                from
                    doc_text_revisions tr
                    join doc_text_elements te on te.id = tr.text_element
                    join users u on tr.author = u.id
                where
                    te.document = $1 :: int4
                    and te.id = $2 :: int4
                    and ($3 :: int4? is null or tr.id = $3 :: int4?)
                order by
                    tr.creation_ts desc
                limit 1
            |]

uncurryTextRevisionRef :: TextRevisionRef -> (Int32, Int32, Maybe Int32)
uncurryTextRevisionRef (TextRevisionRef (TextElementRef docID textID) revision) =
    ( unDocumentID docID
    , unTextElementID textID
    , unTextRevisionID <$> specificTextRevision revision
    )

getTreeNode :: Statement Hash NodeHeader
getTreeNode =
    lmap
        unHash
        $ rmap
            (uncurryN NodeHeader)
            [singletonStatement|
            select
                kind :: text,
                type :: text
            from
                doc_tree_nodes
            where
                hash = $1 :: bytea
        |]

putTreeNode :: Statement (Hash, NodeHeader) ()
putTreeNode =
    lmap
        ( \(hash, header) ->
            ( unHash hash
            , Tree.headerKind header
            , Tree.headerType header
            )
        )
        [resultlessStatement|
            insert into doc_tree_nodes
                (hash, kind, type)
            values
                ($1 :: bytea, $2 :: text, $3 :: text)
            on conflict do nothing
        |]

uncurryTreeEdge
    :: TreeEdge
    -> (ByteString, Int32, Text, Maybe ByteString, Maybe Int32)
uncurryTreeEdge edge =
    ( unHash (TreeEdge.parentHash edge)
    , TreeEdge.position edge
    , TreeEdge.title edge
    , childNode
    , childTextElement
    )
  where
    childNode = case TreeEdge.child edge of
        (TreeEdgeRefToNode hash) -> Just $ unHash hash
        (TreeEdgeRefToTextElement _) -> Nothing
    childTextElement = case TreeEdge.child edge of
        (TreeEdgeRefToTextElement textID) -> Just $ unTextElementID textID
        (TreeEdgeRefToNode _) -> Nothing

putTreeEdge :: Statement TreeEdge ()
putTreeEdge =
    lmap
        uncurryTreeEdge
        [resultlessStatement|
            insert into doc_tree_edges
                ( parent
                , position
                , title
                , child_node
                , child_text_element
                )
            values
                ( $1 :: bytea
                , $2 :: int4
                , $3 :: text
                , $4 :: bytea?
                , $5 :: int4?
                )
            on conflict (parent, position) do update
            set title = EXCLUDED.title
        |]

uncurryTreeEdgeChild
    :: ( Text
       , Maybe ByteString
       , Maybe Text
       , Maybe Text
       , Maybe Int32
       , Maybe Text
       )
    -> Maybe (Text, TreeEdgeChild)
uncurryTreeEdgeChild (title, nodeHash, nodeKind, nodeType, textID, textKind) =
    (title,) <$> (maybeNode <|> maybeText)
  where
    maybeNode = do
        hash <- nodeHash
        kind <- nodeKind
        type_ <- nodeType
        return $
            TreeEdgeToNode
                (Hash hash)
                NodeHeader
                    { Tree.headerKind = kind
                    , Tree.headerType = type_
                    }
    maybeText = do
        id_ <- textID
        kind <- textKind
        return $
            TreeEdgeToTextElement
                TextElement
                    { TextElement.identifier = TextElementID id_
                    , TextElement.kind = kind
                    }

getTreeEdgesByParent :: Statement Hash (Vector (Text, TreeEdgeChild))
getTreeEdgesByParent =
    lmap
        unHash
        $ rmap
            (mapMaybe uncurryTreeEdgeChild)
            [vectorStatement|
                select
                    e.title :: text,
                    n.hash :: bytea?,
                    n.kind :: text?,
                    n.type :: text?,
                    t.id :: int4?,
                    t.kind :: text?
                from
                    doc_tree_edges e
                    left join doc_tree_nodes n on e.child_node = n.hash
                    left join doc_text_elements t on e.child_text_element = t.id
                where
                    e.parent = $1 :: bytea
                order by
                    e.position ASC
            |]

uncurryTreeRevisionHeader
    :: (Int32, UTCTime, UserID, Text) -> TreeRevisionHeader
uncurryTreeRevisionHeader (id_, timestamp, authorID, authorName) =
    TreeRevisionHeader
        { TreeRevision.identifier = TreeRevisionID id_
        , TreeRevision.timestamp = timestamp
        , TreeRevision.author =
            UserRef
                { UserRef.identifier = authorID
                , UserRef.name = authorName
                }
        }

uncurryTreeRevision
    :: (Int32, UTCTime, UserID, Text) -> Node a -> TreeRevision a
uncurryTreeRevision = TreeRevision . uncurryTreeRevisionHeader

uncurryTreeRevisionWithRoot
    :: (Int32, UTCTime, UserID, Text, ByteString)
    -> (Hash, Node a -> TreeRevision a)
uncurryTreeRevisionWithRoot (id_, ts, authorID, authorName, root) =
    (Hash root, uncurryTreeRevision (id_, ts, authorID, authorName))

putTreeRevision
    :: Statement (DocumentID, UserID, Hash) (Node a -> TreeRevision a)
putTreeRevision =
    lmap
        mapInput
        $ rmap
            uncurryTreeRevision
            [singletonStatement|
                WITH inserted AS (
                    insert into doc_tree_revisions
                        (document, author, root)
                    values
                        ($1 :: int4, $2 :: uuid, $3 :: bytea)
                    returning
                        id :: int4,
                        creation_ts :: timestamptz,
                        author :: uuid
                )
                SELECT
                    inserted.id :: int4,
                    inserted.creation_ts :: timestamptz,
                    inserted.author :: uuid,
                    users.name :: text
                FROM
                    inserted
                    JOIN users ON users.id = inserted.author
            |]
  where
    mapInput (docID, userID, rootHash) =
        (unDocumentID docID, userID, unHash rootHash)

getTreeRevision
    :: Statement TreeRevisionRef (Maybe (Hash, Node a -> TreeRevision a))
getTreeRevision =
    lmap
        uncurryTreeRevisionRef
        $ rmap
            (uncurryTreeRevisionWithRoot <$>)
            [maybeStatement|
                select
                    tr.id :: int4,
                    tr.creation_ts :: timestamptz,
                    tr.author :: uuid,
                    u.name :: text,
                    tr.root :: bytea
                from
                    doc_tree_revisions tr
                    join users u on tr.author = u.id
                where
                    tr.document = $1 :: int4
                    and ($2 :: int4? is null or tr.id = $2 :: int4?)
                order by
                    tr.creation_ts desc
                limit 1
            |]

uncurryTreeRevisionRef :: TreeRevisionRef -> (Int32, Maybe Int32)
uncurryTreeRevisionRef (TreeRevisionRef docID selector) =
    (unDocumentID docID, unTreeRevisionID <$> specificTreeRevision selector)

getTreeRevisionHistory
    :: Statement (DocumentID, Maybe UTCTime, Int32) (Vector TreeRevisionHeader)
getTreeRevisionHistory =
    lmap
        mapInput
        $ rmap
            (uncurryTreeRevisionHeader <$>)
            [vectorStatement|
                SELECT
                    tr.id :: INT4,
                    tr.creation_ts :: TIMESTAMPTZ,
                    tr.author :: UUID,
                    u.name :: TEXT
                FROM
                    doc_tree_revisions tr
                    JOIN users u on tr.author = u.id
                WHERE
                    tr.document = $1 :: INT4
                    AND tr.creation_ts < COALESCE($2 :: TIMESTAMPTZ?, NOW())
                ORDER BY
                    tr.creation_ts DESC
                LIMIT
                    $3 :: INT4
            |]
  where
    mapInput (docID, time, limit) = (unDocumentID docID, time, limit)

getTextElementIDsForDocument :: Statement DocumentID (Vector TextElementID)
getTextElementIDsForDocument =
    lmap unDocumentID $
        rmap
            (TextElementID <$>)
            [vectorStatement|
                select
                    id :: int4
                from
                    doc_text_elements
                where
                    document = $1 :: int4
            |]

uncurryHistoryItem
    :: (Maybe Int32, Int32, UTCTime, UserID, Text) -> DocumentHistoryItem
uncurryHistoryItem (textID, revID, ts, authorID, authorName) =
    case textID of
        Just id_ ->
            DocumentHistory.Text
                (TextElementID id_)
                TextRevisionHeader
                    { TextRevision.identifier = TextRevisionID revID
                    , TextRevision.timestamp = ts
                    , TextRevision.author =
                        UserRef
                            { UserRef.identifier = authorID
                            , UserRef.name = authorName
                            }
                    }
        Nothing ->
            DocumentHistory.Tree
                TreeRevisionHeader
                    { TreeRevision.identifier = TreeRevisionID revID
                    , TreeRevision.timestamp = ts
                    , TreeRevision.author =
                        UserRef
                            { UserRef.identifier = authorID
                            , UserRef.name = authorName
                            }
                    }

getDocumentRevisionHistory
    :: Statement (DocumentID, Maybe UTCTime, Int32) (Vector DocumentHistoryItem)
getDocumentRevisionHistory =
    lmap
        mapInput
        $ rmap
            (uncurryHistoryItem <$>)
            [vectorStatement|
                SELECT
                    dr.text_element :: INT4?,
                    dr.id :: INT4,
                    dr.creation_ts :: TIMESTAMPTZ,
                    dr.author :: UUID,
                    u.name :: TEXT
                FROM
                    doc_revisions dr
                    JOIN users u ON dr.author = u.id
                WHERE
                    dr.document = $1 :: INT4
                    AND dr.creation_ts < COALESCE($2 :: TIMESTAMPTZ?, NOW())
                ORDER BY
                    dr.creation_ts DESC
                LIMIT
                    $3 :: INT4
            |]
  where
    mapInput (docID, time, limit) = (unDocumentID docID, time, limit)

-- Natürlich schreibe ich dir einen Kommentar, der sagt, dass hier das UserManagement beginnt!

hasPermission :: Statement (UserID, DocumentID, Permission) Bool
hasPermission =
    lmap
        mapInput
        [singletonStatement|
            select exists (
                SELECT
                    1
                FROM
                    roles r
                JOIN
                    docs d ON d."group" = r.group_id
                    LEFT JOIN external_document_rights e ON e.user_id = r.user_id
                WHERE
                    r.user_id = $1 :: uuid
                    AND (
                        d.id = $2 :: int4
                        OR (e.document_id = $2 :: int4 AND e.permission >= ($3 :: text :: permission))
                    )
            ) :: bool
        |]
  where
    mapInput :: (UserID, DocumentID, Permission) -> (UUID, Int32, Text)
    mapInput (userID, docID, perms) =
        (userID, unDocumentID docID, Text.pack $ show perms)

isGroupAdmin :: Statement (UserID, GroupID) Bool
isGroupAdmin =
    [singletonStatement|
        select exists (
            SELECT
                1
            FROM
                roles
            WHERE
                user_id = $1 :: uuid AND group_id = $2 :: int AND role = 'admin'
        ) :: bool
    |]
