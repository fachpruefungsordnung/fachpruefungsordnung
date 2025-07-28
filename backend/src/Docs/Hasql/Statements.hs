{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Docs.Hasql.Statements
    ( createDocument
    , getDocument
    , getDocuments
    , createTextElement
    , getTextElement
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
import Data.Bifunctor (bimap, first)
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

existsDocument :: Statement DocumentID Bool
existsDocument =
    lmap
        unDocumentID
        [singletonStatement|
            SELECT
                1 :: bool
            FROM
                docs
            WHERE
                id = $1 :: int4
        |]

existsTreeRevision :: Statement TreeRevisionRef Bool
existsTreeRevision =
    lmap
        uncurryTreeRevisionRef
        [singletonStatement|
            SELECT
                1 :: bool
            FROM
                doc_tree_revisions
            WHERE
                document = $1 :: int4
                AND ($2 :: int4? IS NULL OR id = $2 :: int4?)
        |]

existsTextElement :: Statement TextElementRef Bool
existsTextElement =
    lmap
        uncurryTextElementRef
        [singletonStatement|
            SELECT
                1 :: bool
            FROM
                doc_text_elements
            WHERE
                document = $1 :: int4
                AND id = $2 :: int4
        |]

existsTextRevision :: Statement TextRevisionRef Bool
existsTextRevision =
    lmap
        uncurryTextRevisionRef
        [singletonStatement|
            SELECT
                1 :: bool
            FROM
                doc_text_revisions tr
                JOIN doc_text_elements te on tr.text_element = te.id
            WHERE
                te.document = $1 :: int4
                AND tr.text_element = $2 :: int4
                AND ($3 :: int4? IS NULL OR tr.id = $3 :: int4?)
        |]

uncurryDocument
    :: (Int32, Text, Int32, Maybe UTCTime, Maybe UUID, Maybe Text) -> Document
uncurryDocument (id_, name, groupID, lastEdited, lastEditedByID, lastEditedByName) =
    Document
        { Document.identifier = DocumentID id_
        , Document.name = name
        , Document.group = groupID
        , Document.lastEdited = lastEdited
        , Document.lastEditedBy = do
            userID <- lastEditedByID
            userName <- lastEditedByName
            return $
                UserRef
                    { UserRef.identifier = userID
                    , UserRef.name = userName
                    }
        }

createDocument :: Statement (Text, GroupID) Document
createDocument =
    rmap
        uncurryDocument
        [singletonStatement|
            insert into docs
                (name, "group")
            values
                ($1 :: text, $2 :: int4)
            returning
                id :: int4,
                name :: text,
                "group" :: int4,
                NULL :: timestamptz?,
                NULL :: uuid?,
                NULL :: text?
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
                    r.creation_ts :: timestamptz?,
                    r.author_id :: uuid?,
                    r.author_name :: text?
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
                WHERE
                    d.id = $1 :: int4
            |]

getDocuments :: Statement UserID (Vector Document)
getDocuments =
    rmap
        (uncurryDocument <$>)
        [vectorStatement|
            SELECT
                d.id :: int4,
                d.name :: text,
                d."group" :: int4,
                dr.creation_ts :: timestamptz?,
                dr.author_id :: uuid?,
                dr.author_name :: text?
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
            WHERE
                r.user_id = $1 :: uuid
                OR edr.user_id = $1 :: uuid
            ORDER BY
                dr.creation_ts DESC
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

uncurryTextRevisionHeader :: (Int32, UTCTime, UUID) -> TextRevisionHeader
uncurryTextRevisionHeader (id_, timestamp, author) =
    TextRevisionHeader
        { TextRevision.identifier = TextRevisionID id_
        , TextRevision.timestamp = timestamp
        , TextRevision.author = author
        }

uncurryTextRevision :: (Int32, UTCTime, UUID, Text) -> TextRevision
uncurryTextRevision (id_, timestamp, author, content) =
    TextRevision
        (uncurryTextRevisionHeader (id_, timestamp, author))
        content

uncurryTextElementRevision
    :: (Int32, TextElementKind, Maybe Int32, Maybe UTCTime, Maybe UUID, Maybe Text)
    -> TextElementRevision
uncurryTextElementRevision (id_, kind, revisionID, timestamp, author, content) =
    TextElementRevision
        TextElement
            { TextElement.identifier = TextElementID id_
            , TextElement.kind = kind
            }
        $ do
            trRevisionID <- revisionID
            trTimestamp <- timestamp
            trAuthor <- author
            TextRevision
                TextRevisionHeader
                    { TextRevision.identifier = TextRevisionID trRevisionID
                    , TextRevision.timestamp = trTimestamp
                    , TextRevision.author = trAuthor
                    }
                <$> content

createTextRevision :: Statement (TextElementID, UUID, Text) TextRevision
createTextRevision =
    lmap
        mapInput
        $ rmap
            uncurryTextRevision
            [singletonStatement|
                insert into doc_text_revisions
                    (text_element, author, content)
                values
                    ($1 :: int4, $2 :: uuid, $3 :: text)
                returning
                    id :: int4,
                    creation_ts :: timestamptz,
                    author :: uuid,
                    content :: text
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
                    id :: int4,
                    creation_ts :: timestamptz,
                    author :: uuid,
                    content :: text
                from
                    doc_text_revisions
                where
                    text_element = $1 :: int4
                    and ($2 :: int4? is null or id = $2 :: int4?)
                order by
                    creation_ts desc
                limit 1
            |]

getTextRevisionHistory
    :: Statement (TextElementRef, Maybe UTCTime) (Vector TextRevisionHeader)
getTextRevisionHistory =
    lmap
        mapInput
        $ rmap
            (uncurryTextRevisionHeader <$>)
            [vectorStatement|
                SELECT
                    tr.id :: INT4,
                    tr.creation_ts :: TIMESTAMPTZ,
                    tr.author :: UUID
                FROM
                    doc_text_revisions tr
                    JOIN doc_text_elements te ON tr.text_element = te.id
                WHERE
                    te.document = $1 :: INT4
                    AND tr.text_element = $2 :: INT4
                    AND tr.creation_ts < COALESCE($3 :: TIMESTAMPTZ?, NOW())
                ORDER BY
                    tr.creation_ts DESC
                LIMIT
                    10
            |]
  where
    mapInput (TextElementRef docID textID, maybeTimestamp) =
        (unDocumentID docID, unTextElementID textID, maybeTimestamp)

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
                    tr.content :: text?
                from
                    doc_text_revisions tr
                    join doc_text_elements te on te.id = tr.text_element
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
            on conflict do nothing
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
                    join doc_tree_nodes n on e.child_node = n.hash
                    join doc_text_elements t on e.child_text_element = t.id
                where
                    e.parent = $1 :: bytea
            |]

uncurryTreeRevisionHeader :: (Int32, UTCTime, UserID) -> TreeRevisionHeader
uncurryTreeRevisionHeader (id_, timestamp, author) =
    TreeRevisionHeader
        { TreeRevision.identifier = TreeRevisionID id_
        , TreeRevision.timestamp = timestamp
        , TreeRevision.author = author
        }

uncurryTreeRevision :: (Int32, UTCTime, UserID) -> Node a -> TreeRevision a
uncurryTreeRevision = TreeRevision . uncurryTreeRevisionHeader

uncurryTreeRevisionWithRoot
    :: (Int32, UTCTime, UserID, ByteString)
    -> (Hash, Node a -> TreeRevision a)
uncurryTreeRevisionWithRoot (id_, ts, author, root) =
    (Hash root, uncurryTreeRevision (id_, ts, author))

putTreeRevision
    :: Statement (DocumentID, UserID, Hash) (Node a -> TreeRevision a)
putTreeRevision =
    lmap
        mapInput
        $ rmap
            uncurryTreeRevision
            [singletonStatement|
                insert into doc_tree_revisions
                    (document, author, root)
                values
                    ($1 :: int4, $2 :: uuid, $3 :: bytea)
                returning
                    id :: int4,
                    creation_ts :: timestamptz,
                    author :: uuid
            |]
  where
    mapInput (docID, userID, rootHash) =
        (unDocumentID docID, userID, unHash rootHash)

getTreeRevision
    :: Statement TreeRevisionRef (Hash, Node a -> TreeRevision a)
getTreeRevision =
    lmap
        uncurryTreeRevisionRef
        $ rmap
            uncurryTreeRevisionWithRoot
            [singletonStatement|
                select
                    id :: int4,
                    creation_ts :: timestamptz,
                    author :: uuid,
                    root :: bytea
                from
                    doc_tree_revisions
                where
                    document = $1 :: int4
                    and ($2 :: int4? is null or id = $2 :: int4?)
                order by
                    creation_ts desc
                limit 1
            |]

uncurryTreeRevisionRef :: TreeRevisionRef -> (Int32, Maybe Int32)
uncurryTreeRevisionRef (TreeRevisionRef docID selector) =
    (unDocumentID docID, unTreeRevisionID <$> specificTreeRevision selector)

getTreeRevisionHistory
    :: Statement (DocumentID, Maybe UTCTime) (Vector TreeRevisionHeader)
getTreeRevisionHistory =
    lmap
        (first unDocumentID)
        $ rmap
            (uncurryTreeRevisionHeader <$>)
            [vectorStatement|
                SELECT
                    id :: INT4,
                    creation_ts :: TIMESTAMPTZ,
                    author :: UUID
                FROM
                    doc_tree_revisions
                WHERE
                    document = $1 :: INT4
                    AND creation_ts < COALESCE($2 :: TIMESTAMPTZ?, NOW())
                ORDER BY
                    creation_ts DESC
                LIMIT
                    10
            |]

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
    :: (Maybe Int32, Int32, UTCTime, UserID) -> DocumentHistoryItem
uncurryHistoryItem (textID, revID, ts, authorID) =
    case textID of
        Just id_ ->
            DocumentHistory.Text
                (TextElementID id_)
                TextRevisionHeader
                    { TextRevision.identifier = TextRevisionID revID
                    , TextRevision.timestamp = ts
                    , TextRevision.author = authorID
                    }
        Nothing ->
            DocumentHistory.Tree
                TreeRevisionHeader
                    { TreeRevision.identifier = TreeRevisionID revID
                    , TreeRevision.timestamp = ts
                    , TreeRevision.author = authorID
                    }

getDocumentRevisionHistory
    :: Statement (DocumentID, Maybe UTCTime) (Vector DocumentHistoryItem)
getDocumentRevisionHistory =
    lmap
        (first unDocumentID)
        $ rmap
            (uncurryHistoryItem <$>)
            [vectorStatement|
                SELECT
                    text_element :: INT4?,
                    id :: INT4,
                    creation_ts :: TIMESTAMPTZ,
                    author :: UUID
                FROM
                    doc_revisions
                WHERE
                    document = $1 :: INT4
                    AND creation_ts < COALESCE($2 :: TIMESTAMPTZ?, NOW())
                ORDER BY
                    creation_ts DESC
                LIMIT
                    10
            |]

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
