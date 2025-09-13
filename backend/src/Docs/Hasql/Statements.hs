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
    , createComment
    , getComments
    , createReply
    , getReplies
    , putCommentAnchor
    , getCommentAnchors
    , deleteCommentAnchorsExcept
    , resolveComment
    , existsComment
    , getLogs
    , logMessage
    , getRevisionKey
    , updateLatestTitle
    , createDraftTextRevision
    , getDraftTextRevision
    , deleteDraftTextRevision
    , getDraftCommentAnchors
    , putDraftCommentAnchors
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
import GHC.Int (Int64)

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

import Data.Aeson (ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (parseJSON, parseMaybe)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Docs.Comment
    ( Anchor (Anchor)
    , Comment (Comment)
    , CommentAnchor (CommentAnchor)
    , CommentID (CommentID, unCommentID)
    , CommentRef (CommentRef)
    , Message (Message)
    )
import qualified Docs.Comment as Comment
import Docs.Document (Document (Document), DocumentID (..))
import qualified Docs.Document as Document
import Docs.DocumentHistory (DocumentHistoryItem)
import qualified Docs.DocumentHistory as DocumentHistory
import Docs.Hash (Hash (..))
import Docs.Hasql.TreeEdge
    ( TreeEdge
    , TreeEdgeChild (..)
    , TreeEdgeChildRef (..)
    )
import qualified Docs.Hasql.TreeEdge as TreeEdge
import Docs.Revision
    ( RevisionID (unRevisionID)
    , RevisionKey (RevisionKey)
    , RevisionSelector
    , latestRevisionAsOf
    , specificRevision
    )
import qualified Docs.Revision as Revision
import Docs.TextElement
    ( TextElement (TextElement)
    , TextElementID (..)
    , TextElementKind
    , TextElementRef (..)
    , TextElementType
    )
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( DraftRevision (DraftRevision)
    , DraftRevisionHeader (DraftRevisionHeader)
    , DraftRevisionID (..)
    , TextElementRevision (TextElementRevision)
    , TextRevision (TextRevision)
    , TextRevisionHeader (TextRevisionHeader)
    , TextRevisionID (..)
    , TextRevisionRef (..)
    , TextRevisionSelector (..)
    , latestTextRevisionAsOf
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
    , latestTreeRevisionAsOf
    , specificTreeRevision
    )
import qualified Docs.TreeRevision as TreeRevision
import Docs.UserRef (UserRef (UserRef))
import qualified Docs.UserRef as UserRef
import Logging.Logs (LogMessage (LogMessage), Severity (..), Source (..))
import qualified Logging.Logs as Logs
import Logging.Scope (Scope (Scope, unScope))

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
                    id = $1 :: int8
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
                    document = $1 :: int8
                    AND ($2 :: int8? IS NULL OR id = $2 :: int8?)
                    AND ($3 :: timestamptz? IS NULL OR creation_ts <= $3 :: timestamptz?)
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
                    document = $1 :: int8
                    AND id = $2 :: int8
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
                    te.document = $1 :: int8
                    AND tr.text_element = $2 :: int8
                    AND ($3 :: int8? IS NULL OR tr.id = $3 :: int8?)
                    AND ($4 :: timestamptz? IS NULL OR tr.creation_ts <= $4 :: timestamptz?)
            ) :: bool
        |]

uncurryDocument
    :: ( Int64
       , Text
       , Int64
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
                    ($1 :: text, $2 :: int8, $3 :: uuid)
                returning
                    id :: int8,
                    name :: text,
                    "group" :: int8,
                    creation_ts :: timestamptz?,
                    created_by :: uuid?
            )
            SELECT
                inserted.id :: int8,
                inserted.name :: text,
                inserted."group" :: int8,
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
                    d.id :: int8,
                    d.name :: text,
                    d."group" :: int8,
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
                    d.id = $1 :: int8
            |]

getDocuments :: Statement UserID (Vector Document)
getDocuments =
    rmap
        (uncurryDocument <$>)
        [vectorStatement|
            SELECT DISTINCT
                d.id :: int8,
                d.name :: text,
                d."group" :: int8,
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
                d.id :: int8,
                d.name :: text,
                d."group" :: int8,
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
                OR d."group" = $2 :: int8?
            ORDER BY
                last_edited DESC
        |]

uncurryTextElement :: (Int64, Text, Text) -> TextElement
uncurryTextElement (id_, kind, type_) =
    TextElement
        { TextElement.identifier = TextElementID id_
        , TextElement.textElementKind = kind
        , TextElement.textElementType = type_
        }

createTextElement :: Statement (DocumentID, Text, Text) TextElement
createTextElement =
    lmap mapInput $
        rmap
            uncurryTextElement
            [singletonStatement|
            insert into doc_text_elements
                (document, kind, type)
            values
                ($1 :: int8, $2 :: text, $3 :: text)
            returning
                id :: int8,
                kind :: text,
                type :: text
        |]
  where
    mapInput (docID, kind, type_) = (unDocumentID docID, kind, type_)

getTextElement :: Statement TextElementID (Maybe TextElement)
getTextElement =
    lmap
        unTextElementID
        $ rmap
            (uncurryTextElement <$>)
            [maybeStatement|
                select
                    id :: int8,
                    kind :: text,
                    type :: text
                from
                    doc_text_elements
                where
                    id = $1 :: int8
            |]

uncurryTextRevisionHeader :: (Int64, UTCTime, UUID, Text) -> TextRevisionHeader
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

uncurryTextRevision
    :: (Monad m)
    => (Int64, UTCTime, UUID, Text, Text)
    -> (TextRevisionID -> m (Vector CommentAnchor))
    -> m TextRevision
uncurryTextRevision (id_, timestamp, authorID, authorName, content) getAnchors =
    getAnchors (TextRevisionID id_)
        <&> TextRevision
            (uncurryTextRevisionHeader (id_, timestamp, authorID, authorName))
            content

uncurryTextElementRevision
    :: (Monad m)
    => ( Int64
       , TextElementKind
       , TextElementType
       , Maybe Int64
       , Maybe UTCTime
       , Maybe UUID
       , Maybe Text
       , Maybe Text
       )
    -> (TextRevisionID -> m (Vector CommentAnchor))
    -> m TextElementRevision
uncurryTextElementRevision
    (id_, kind, type_, revisionID, timestamp, authorID, authorName, content)
    getAnchors = do
        anchors <- mapM (getAnchors . TextRevisionID) revisionID
        return
            $ TextElementRevision
                TextElement
                    { TextElement.identifier = TextElementID id_
                    , TextElement.textElementKind = kind
                    , TextElement.textElementType = type_
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
                    <*> anchors

updateTextRevision
    :: (Monad m)
    => Statement
        (TextRevisionID, Text)
        ( (TextRevisionID -> m (Vector CommentAnchor))
          -> m TextRevision
        )
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
                        id = $1 :: int8
                    RETURNING
                        id :: int8,
                        creation_ts :: timestamptz,
                        author :: uuid,
                        content :: text
                ),
                updated_anchors AS (
                    UPDATE
                        doc_comment_anchors
                    SET
                        comment = updated.id
                    FROM
                        updated
                    WHERE
                        doc_comment_anchors.comment = $1 :: int8
                    RETURNING
                        doc_comment_anchors.id
                )
                SELECT
                    updated.id :: int8,
                    updated.creation_ts :: timestamptz,
                    updated.author :: uuid,
                    users.name :: text,
                    updated.content :: text
                FROM
                    updated
                    JOIN users on users.id = updated.author
            |]

createTextRevision
    :: (Monad m)
    => Statement
        (TextElementID, UUID, Text)
        ((TextRevisionID -> m (Vector CommentAnchor)) -> m TextRevision)
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
                        ($1 :: int8, $2 :: uuid, $3 :: text)
                    returning
                        id :: int8,
                        creation_ts :: timestamptz,
                        author :: uuid,
                        content :: text
                )
                SELECT
                    inserted.id :: int8,
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
    :: (Monad m)
    => Statement
        (TextElementID, TextRevisionSelector)
        ((TextRevisionID -> m (Vector CommentAnchor)) -> m (Maybe TextRevision))
getTextRevision =
    lmap
        mapInput
        $ rmap
            (\row f -> mapM (`uncurryTextRevision` f) row)
            [maybeStatement|
                select
                    tr.id :: int8,
                    tr.creation_ts :: timestamptz,
                    tr.author :: uuid,
                    u.name :: text,
                    tr.content :: text
                from
                    doc_text_revisions tr
                    join users u on tr.author = u.id
                where
                    tr.text_element = $1 :: int8
                    and ($2 :: int8? is null or tr.id = $2 :: int8?)
                    and ($3 :: timestamptz? is null or tr.creation_ts <= $3 :: timestamptz?)
                order by
                    tr.creation_ts desc
                limit 1
            |]
  where
    mapInput (textID, selector) =
        ( unTextElementID textID
        , unTextRevisionID <$> specificTextRevision selector
        , latestTextRevisionAsOf selector
        )

getTextRevisionHistory
    :: Statement
        (TextElementRef, Maybe UTCTime, Maybe UTCTime, Int64)
        (Vector TextRevisionHeader)
getTextRevisionHistory =
    lmap
        mapInput
        $ rmap
            (uncurryTextRevisionHeader <$>)
            [vectorStatement|
                SELECT
                    tr.id :: int8,
                    tr.creation_ts :: TIMESTAMPTZ,
                    tr.author :: UUID,
                    u.name :: TEXT
                FROM
                    doc_text_revisions tr
                    JOIN doc_text_elements te ON tr.text_element = te.id
                    JOIN users u on tr.author = u.id
                WHERE
                    te.document = $1 :: int8
                    AND tr.text_element = $2 :: int8
                    AND tr.creation_ts > COALESCE($3 :: TIMESTAMPTZ?, '1900-01-01 00:00:00+00'::TIMESTAMPTZ)
                    AND tr.creation_ts < COALESCE($4 :: TIMESTAMPTZ?, NOW())
                ORDER BY
                    tr.creation_ts DESC
                LIMIT
                    $5 :: int8
            |]
  where
    mapInput (TextElementRef docID textID, maybeFrom, maybeTo, limit) =
        (unDocumentID docID, unTextElementID textID, maybeFrom, maybeTo, limit)

getLatestTextRevisionID :: Statement TextElementRef (Maybe TextRevisionID)
getLatestTextRevisionID =
    lmap
        uncurryTextElementRef
        $ rmap
            (TextRevisionID <$>)
            [maybeStatement|
                select
                    tr.id :: int8
                from
                    doc_text_revisions tr
                    join doc_text_elements te on tr.text_element = te.id
                where
                    te.document = $1 :: int8
                    and tr.text_element = $2 :: int8
                order by
                    tr.creation_ts desc
                limit 1
            |]

uncurryTextElementRef :: TextElementRef -> (Int64, Int64)
uncurryTextElementRef (TextElementRef docID textID) =
    (unDocumentID docID, unTextElementID textID)

getTextElementRevision
    :: (Monad m)
    => Statement
        TextRevisionRef
        ((TextRevisionID -> m (Vector CommentAnchor)) -> m (Maybe TextElementRevision))
getTextElementRevision =
    lmap
        uncurryTextRevisionRef
        $ rmap
            (\row f -> mapM (`uncurryTextElementRevision` f) row)
            [maybeStatement|
                select
                    te.id :: int8,
                    te.kind :: text,
                    te.type :: text,
                    tr.id :: int8?,
                    tr.creation_ts :: timestamptz?,
                    tr.author :: uuid?,
                    u.name :: text?,
                    tr.content :: text?
                from
                    doc_text_revisions tr
                    join doc_text_elements te on te.id = tr.text_element
                    join users u on tr.author = u.id
                where
                    te.document = $1 :: int8
                    and te.id = $2 :: int8
                    and ($3 :: int8? is null or tr.id = $3 :: int8?)
                    and ($4 :: timestamptz? is null or tr.creation_ts <= $4 :: timestamptz?)
                order by
                    tr.creation_ts desc
                limit 1
            |]

uncurryTextRevisionRef
    :: TextRevisionRef -> (Int64, Int64, Maybe Int64, Maybe UTCTime)
uncurryTextRevisionRef (TextRevisionRef (TextElementRef docID textID) revision) =
    ( unDocumentID docID
    , unTextElementID textID
    , unTextRevisionID <$> specificTextRevision revision
    , latestTextRevisionAsOf revision
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
                type :: text,
                heading:: text?
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
            , Tree.heading header
            )
        )
        [resultlessStatement|
            insert into doc_tree_nodes
                (hash, kind, type, heading)
            values
                ($1 :: bytea, $2 :: text, $3 :: text, $4 :: text?)
            on conflict do nothing
        |]

uncurryTreeEdge
    :: TreeEdge
    -> (ByteString, Int64, Maybe ByteString, Maybe Int64)
uncurryTreeEdge edge =
    ( unHash (TreeEdge.parentHash edge)
    , TreeEdge.position edge
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
                , child_node
                , child_text_element
                )
            values
                ( $1 :: bytea
                , $2 :: int8
                , $3 :: bytea?
                , $4 :: int8?
                )
            on conflict (parent, position) do nothing
        |]

uncurryTreeEdgeChild
    :: ( Maybe ByteString
       , Maybe Text
       , Maybe Text
       , Maybe Text
       , Maybe Int64
       , Maybe Text
       , Maybe Text
       )
    -> Maybe TreeEdgeChild
uncurryTreeEdgeChild (nodeHash, nodeKind, nodeType, nodeHeading, textID, textKind, textType) =
    maybeNode <|> maybeText
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
                    , Tree.heading = nodeHeading
                    }
    maybeText = do
        id_ <- textID
        kind <- textKind
        type_ <- textType
        return $
            TreeEdgeToTextElement
                TextElement
                    { TextElement.identifier = TextElementID id_
                    , TextElement.textElementKind = kind
                    , TextElement.textElementType = type_
                    }

getTreeEdgesByParent :: Statement Hash (Vector TreeEdgeChild)
getTreeEdgesByParent =
    lmap
        unHash
        $ rmap
            (mapMaybe uncurryTreeEdgeChild)
            [vectorStatement|
                select
                    n.hash :: bytea?,
                    n.kind :: text?,
                    n.type :: text?,
                    n.heading :: text?,
                    t.id :: int8?,
                    t.kind :: text?,
                    t.type :: text?
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
    :: (Int64, UTCTime, UserID, Text) -> TreeRevisionHeader
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
    :: (Int64, UTCTime, UserID, Text) -> Node a -> TreeRevision a
uncurryTreeRevision = TreeRevision . uncurryTreeRevisionHeader

uncurryTreeRevisionWithRoot
    :: (Int64, UTCTime, UserID, Text, ByteString)
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
                        ($1 :: int8, $2 :: uuid, $3 :: bytea)
                    returning
                        id :: int8,
                        creation_ts :: timestamptz,
                        author :: uuid
                )
                SELECT
                    inserted.id :: int8,
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
                    tr.id :: int8,
                    tr.creation_ts :: timestamptz,
                    tr.author :: uuid,
                    u.name :: text,
                    tr.root :: bytea
                from
                    doc_tree_revisions tr
                    join users u on tr.author = u.id
                where
                    tr.document = $1 :: int8
                    and ($2 :: int8? is null or tr.id = $2 :: int8?)
                    and ($3 :: timestamptz? is null or tr.creation_ts <= $3 :: timestamptz?)
                order by
                    tr.creation_ts desc
                limit 1
            |]

uncurryTreeRevisionRef :: TreeRevisionRef -> (Int64, Maybe Int64, Maybe UTCTime)
uncurryTreeRevisionRef (TreeRevisionRef docID selector) =
    ( unDocumentID docID
    , unTreeRevisionID <$> specificTreeRevision selector
    , latestTreeRevisionAsOf selector
    )

getTreeRevisionHistory
    :: Statement (DocumentID, Maybe UTCTime, Int64) (Vector TreeRevisionHeader)
getTreeRevisionHistory =
    lmap
        mapInput
        $ rmap
            (uncurryTreeRevisionHeader <$>)
            [vectorStatement|
                SELECT
                    tr.id :: int8,
                    tr.creation_ts :: TIMESTAMPTZ,
                    tr.author :: UUID,
                    u.name :: TEXT
                FROM
                    doc_tree_revisions tr
                    JOIN users u on tr.author = u.id
                WHERE
                    tr.document = $1 :: int8
                    AND tr.creation_ts < COALESCE($2 :: TIMESTAMPTZ?, NOW())
                ORDER BY
                    tr.creation_ts DESC
                LIMIT
                    $3 :: int8
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
                    id :: int8
                from
                    doc_text_elements
                where
                    document = $1 :: int8
            |]

uncurryHistoryItem
    :: (Maybe Int64, Int64, UTCTime, UserID, Text) -> DocumentHistoryItem
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
    :: Statement (DocumentID, Maybe UTCTime, Int64) (Vector DocumentHistoryItem)
getDocumentRevisionHistory =
    lmap
        mapInput
        $ rmap
            (uncurryHistoryItem <$>)
            [vectorStatement|
                SELECT
                    dr.text_element :: int8?,
                    dr.id :: int8,
                    dr.creation_ts :: TIMESTAMPTZ,
                    dr.author :: UUID,
                    u.name :: TEXT
                FROM
                    doc_revisions dr
                    JOIN users u ON dr.author = u.id
                WHERE
                    dr.document = $1 :: int8
                    AND dr.creation_ts < COALESCE($2 :: TIMESTAMPTZ?, NOW())
                ORDER BY
                    dr.creation_ts DESC
                LIMIT
                    $3 :: int8
            |]
  where
    mapInput (docID, time, limit) = (unDocumentID docID, time, limit)

uncurryRevisionKey :: (UTCTime, Int64, Maybe Int64, Int64) -> RevisionKey
uncurryRevisionKey (timestamp, docID, maybeTextID, revID) =
    RevisionKey
        { Revision.timestamp = timestamp
        , Revision.revision = maybe tree text maybeTextID
        }
  where
    tree =
        Revision.Tree $
            TreeRevisionRef (DocumentID docID) $
                TreeRevision.Specific $
                    TreeRevisionID revID
    text textID =
        Revision.Text $
            TextRevisionRef (TextElementRef (DocumentID docID) (TextElementID textID)) $
                TextRevision.Specific $
                    TextRevisionID revID

getRevisionKey
    :: Statement
        (DocumentID, RevisionSelector)
        (Maybe RevisionKey)
getRevisionKey =
    lmap mapInput $
        rmap
            (uncurryRevisionKey <$>)
            [maybeStatement|
                SELECT
                    creation_ts :: TIMESTAMPTZ,
                    document :: int8,
                    text_element :: int8?,
                    id :: int8
                FROM
                    doc_revisions
                WHERE
                    document = $1 :: int8
                    AND ($2 :: int8? IS NULL OR id = $2 :: int8?)
                    AND ($3 :: timestamptz? IS NULL OR creation_ts <= $3 :: timestamptz?)
                ORDER BY
                    creation_ts DESC
                LIMIT 1
        |]
  where
    mapInput (docID, selector) =
        ( unDocumentID docID
        , unRevisionID <$> specificRevision selector
        , latestRevisionAsOf selector
        )

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
                        d.id = $2 :: int8
                        OR (e.document_id = $2 :: int8 AND e.permission >= ($3 :: text :: permission))
                    )
            ) :: bool
        |]
  where
    mapInput :: (UserID, DocumentID, Permission) -> (UUID, Int64, Text)
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
                user_id = $1 :: uuid AND group_id = $2 :: int8 AND role = 'admin'
        ) :: bool
    |]

-- comments

uncurryMessage :: (UUID, Text, UTCTime, Text) -> Message
uncurryMessage (authorID, authorName, timestamp, content) =
    Message
        { Comment.author =
            UserRef
                { UserRef.identifier = authorID
                , UserRef.name = authorName
                }
        , Comment.timestamp = timestamp
        , Comment.content = content
        }

uncurryComment
    :: (Int64, UUID, Text, UTCTime, Maybe UTCTime, Text)
    -> Comment
uncurryComment (id_, authorID, authorName, timestamp, resolved, content) =
    Comment
        { Comment.identifier = CommentID id_
        , Comment.status = maybe Comment.Open Comment.Resolved resolved
        , Comment.message = uncurryMessage (authorID, authorName, timestamp, content)
        , Comment.replies = Vector.empty
        }

createComment :: Statement (UserID, TextElementID, Text) Comment
createComment =
    lmap
        mapInput
        $ rmap
            uncurryComment
            [singletonStatement|
                WITH inserted AS (
                    INSERT INTO doc_comments
                        (author, text_element, content)
                    VALUES
                        ($1 :: UUID, $2 :: INT8, $3 :: TEXT)
                    RETURNING
                        id :: INT8,
                        author :: UUID,
                        creation_ts :: TIMESTAMPTZ,
                        resolved_ts :: TIMESTAMPTZ?,
                        content :: TEXT
                )
                SELECT
                    inserted.id :: INT8,
                    users.id :: UUID,
                    users.name :: TEXT,
                    inserted.creation_ts :: TIMESTAMPTZ,
                    inserted.resolved_ts :: TIMESTAMPTZ?,
                    inserted.content :: TEXT
                FROM
                    inserted
                    LEFT JOIN users ON inserted.author = users.id
            |]
  where
    mapInput (userID, textID, text) = (userID, unTextElementID textID, text)

getComments :: Statement (DocumentID, TextElementID) (Vector Comment)
getComments =
    lmap (bimap unDocumentID unTextElementID) $
        rmap
            (uncurryComment <$>)
            [vectorStatement|
                SELECT
                    comments.id :: INT8,
                    users.id :: UUID,
                    users.name :: TEXT,
                    comments.creation_ts :: TIMESTAMPTZ,
                    comments.resolved_ts :: TIMESTAMPTZ?,
                    comments.content :: TEXT
                FROM
                    doc_comments comments
                    LEFT JOIN users ON comments.author = users.id
                    LEFT JOIN doc_text_elements
                        ON comments.text_element = doc_text_elements.id
                WHERE
                    comments.text_element = $2 :: INT8
                    AND doc_text_elements.document = $1 :: INT8
            |]

createReply :: Statement (UserID, CommentID, Text) Message
createReply =
    lmap mapInput $
        rmap
            uncurryMessage
            [singletonStatement|
                WITH inserted AS (
                    INSERT INTO doc_comment_replies
                        (author, comment, content)
                    VALUES
                        ($1 :: UUID, $2 :: INT8, $3 :: TEXT)
                    RETURNING
                        author :: UUID,
                        creation_ts :: TIMESTAMPTZ,
                        content :: TEXT
                )
                SELECT
                    users.id :: UUID,
                    users.name :: TEXT,
                    inserted.creation_ts :: TIMESTAMPTZ,
                    inserted.content :: TEXT
                FROM
                    inserted
                    LEFT JOIN users ON inserted.author = users.id
            |]
  where
    mapInput (userID, commentID, content) =
        ( userID
        , unCommentID commentID
        , content
        )

getReplies :: Statement CommentRef (Vector Message)
getReplies =
    lmap curryCommentRef $
        rmap
            (uncurryMessage <$>)
            [vectorStatement|
                SELECT
                    users.id :: UUID,
                    users.name :: TEXT,
                    replies.creation_ts :: TIMESTAMPTZ,
                    replies.content :: TEXT
                FROM
                    doc_comment_replies replies
                    LEFT JOIN users ON replies.author = users.id
                    LEFT JOIN doc_comments comments ON replies.comment = comments.id
                    LEFT JOIN doc_text_elements text_elements ON comments.text_element = text_elements.id
                WHERE
                    text_elements.document = $1 :: INT8
                    AND comments.text_element = $2 :: INT8
                    AND replies.comment = $3 :: INT8
            |]

curryCommentRef :: CommentRef -> (Int64, Int64, Int64)
curryCommentRef (CommentRef (TextElementRef docID textID) commentID) =
    (unDocumentID docID, unTextElementID textID, unCommentID commentID)

resolveComment :: Statement CommentID ()
resolveComment =
    lmap
        unCommentID
        [resultlessStatement|
        UPDATE
            doc_comments
        SET
            resolved_ts = now()
        WHERE
            id = $1 :: INT8
    |]

existsComment :: Statement CommentRef Bool
existsComment =
    lmap
        mapInput
        [singletonStatement|
            SELECT EXISTS (
                SELECT
                    1
                FROM
                    doc_comments c
                    JOIN doc_text_elements te ON c.text_element = te.id
                WHERE
                    te.document = $1 :: int8
                    AND c.text_element = $2 :: int8
                    AND c.id = $3 :: int8
            ) :: bool
        |]
  where
    mapInput (CommentRef (TextElementRef docID textID) commentID) =
        (unDocumentID docID, unTextElementID textID, unCommentID commentID)

uncurryCommentAnchor :: (Int64, Int64, Int64, Int64, Int64) -> CommentAnchor
uncurryCommentAnchor (comment, start_col, start_row, end_col, end_row) =
    let start = Anchor start_col start_row
        end = Anchor end_col end_row
     in CommentAnchor
            { Comment.comment = CommentID comment
            , Comment.anchor = Comment.range start end
            }

putCommentAnchor :: Statement (TextRevisionID, CommentAnchor) CommentAnchor
putCommentAnchor =
    lmap
        mapInput
        $ rmap
            uncurryCommentAnchor
            [singletonStatement|
                INSERT INTO doc_comment_anchors
                    (revision, comment, start_col, start_row, end_col, end_row)
                VALUES
                    ($1 :: INT8, $2 :: INT8, $3 :: INT8, $4 :: INT8, $5 :: INT8, $6 :: INT8)
                ON CONFLICT (comment, revision)
                DO UPDATE SET
                    start_col = EXCLUDED.start_col,
                    start_row = EXCLUDED.start_col,
                    end_col = EXCLUDED.end_col,
                    end_row = EXCLUDED.end_row
                RETURNING
                    comment :: INT8,
                    start_col :: INT8,
                    start_row :: INT8,
                    end_col :: INT8,
                    end_row :: INT8
            |]
  where
    mapInput (revID, anchor) =
        let range = Comment.anchor anchor
            start = Comment.start range
            end = Comment.end range
         in ( unTextRevisionID revID
            , unCommentID $ Comment.comment anchor
            , Comment.col start
            , Comment.row start
            , Comment.col end
            , Comment.row end
            )

getCommentAnchors :: Statement TextRevisionID (Vector CommentAnchor)
getCommentAnchors =
    lmap unTextRevisionID $
        rmap
            (uncurryCommentAnchor <$>)
            [vectorStatement|
                SELECT
                    comment :: INT8,
                    start_col :: INT8,
                    start_row :: INT8,
                    end_col :: INT8,
                    end_row :: INT8
                FROM
                    doc_comment_anchors
                WHERE
                    revision = $1 :: INT8
                ORDER BY
                    comment, start_col, end_col
            |]

deleteCommentAnchorsExcept :: Statement (TextRevisionID, Vector CommentID) ()
deleteCommentAnchorsExcept =
    lmap
        (bimap unTextRevisionID (unCommentID <$>))
        [resultlessStatement|
            DELETE FROM
                doc_comment_anchors
            WHERE
                revision = $1 :: INT8
                AND (cardinality($2 :: INT8[]) = 0 OR comment <> ALL($2 :: INT8[]))
        |]

uncurryLogMessage
    :: (UUID, Text, UTCTime, Maybe UUID, Maybe Text, Text, Aeson.Value)
    -> LogMessage
uncurryLogMessage (identifier, severity, timestamp, userID, userName, scope, content) =
    LogMessage
        { Logs.identifier = identifier
        , Logs.severity = case severity of
            "info" -> Info
            "warning" -> Warning
            "error" -> Error
            _ -> undefined -- should be unreachable
        , Logs.timestamp = timestamp
        , Logs.source = maybe System User $ do
            id_ <- userID
            name <- userName
            return
                UserRef
                    { UserRef.identifier = id_
                    , UserRef.name = name
                    }
        , Logs.scope = Scope scope
        , Logs.content = content
        }

getLogs :: Statement (Maybe UTCTime, Int64) (Vector LogMessage)
getLogs =
    rmap
        (uncurryLogMessage <$>)
        [vectorStatement|
            SELECT
                logs.id :: UUID,
                logs.severity :: TEXT,
                logs."timestamp" :: TIMESTAMPTZ,
                users.id :: UUID?,
                users.name :: TEXT?,
                logs.scope :: Text,
                logs.content :: JSONB
            FROM
                logs
                LEFT JOIN users ON logs.user = users.id
            WHERE
                logs."timestamp" < COALESCE($1 :: TIMESTAMPTZ?, now())
            ORDER BY
                logs."timestamp" DESC
            LIMIT
                $2 :: INT8
        |]

logMessage
    :: (ToJSON v)
    => Statement (Severity, Maybe UserID, Scope, v) LogMessage
logMessage =
    lmap mapInput $
        rmap
            uncurryLogMessage
            [singletonStatement|
            WITH inserted AS (
                INSERT INTO logs
                    (severity, "user", scope, content)
                VALUES
                    ($1 :: TEXT :: severity, $2 :: UUID?, $3 :: TEXT, $4 :: JSONB)
                RETURNING
                    id :: UUID,
                    "severity" :: TEXT,
                    "timestamp" :: TIMESTAMPTZ,
                    "user" :: UUID?,
                    scope :: Text,
                    content :: JSONB
            )
            SELECT
                inserted.id :: UUID,
                inserted.severity :: TEXT,
                inserted."timestamp" :: TIMESTAMPTZ,
                users.id :: UUID?,
                users.name :: TEXT?,
                inserted.scope :: TEXT,
                inserted.content :: JSONB
            FROM
                inserted
                LEFT JOIN users ON inserted.user = users.id
        |]
  where
    mapInput (severity, source, scope, content) =
        ( case severity of
            Info -> "info"
            Warning -> "warning"
            Error -> "error"
        , source
        , unScope scope
        , toJSON content
        )

updateLatestTitle :: Statement (TextElementID, Text) ()
updateLatestTitle =
    lmap
        (first unTextElementID)
        [resultlessStatement|
            UPDATE
                doc_tree_edges d
            SET
                title = $2::text
            WHERE
                d.ctid = (
                    SELECT
                        ctid
                    FROM
                        doc_tree_edges
                    WHERE
                        child_text_element = $1::bigint
                    ORDER BY
                        creation_ts DESC
                    LIMIT 1
                )
        |]

-- | Helper to construct DraftRevisionHeader from database row
uncurryDraftRevisionHeader
    :: (Int64, Int64, UTCTime, UTCTime, UUID, Text) -> DraftRevisionHeader
uncurryDraftRevisionHeader (draftId, basedOnId, creationTs, updatedTs, authorID, authorName) =
    DraftRevisionHeader
        { TextRevision.draftIdentifier = DraftRevisionID draftId
        , TextRevision.basedOnRevision = TextRevisionID basedOnId
        , TextRevision.creationTimestamp = creationTs
        , TextRevision.lastUpdatedTimestamp = updatedTs
        , TextRevision.draftAuthor =
            UserRef
                { UserRef.identifier = authorID
                , UserRef.name = authorName
                }
        }

-- | Helper to construct DraftRevision from database row
uncurryDraftRevision
    :: (Monad m)
    => (Int64, Int64, UTCTime, UTCTime, UUID, Text, Text)
    -> (DraftRevisionID -> m (Vector CommentAnchor))
    -> m DraftRevision
uncurryDraftRevision (draftId, basedOnId, creationTs, updatedTs, authorID, authorName, content) getAnchors =
    getAnchors (DraftRevisionID draftId)
        <&> DraftRevision
            ( uncurryDraftRevisionHeader
                (draftId, basedOnId, creationTs, updatedTs, authorID, authorName)
            )
            content

-- | Create a new draft text revision
createDraftTextRevision
    :: (Monad m)
    => Statement
        (TextElementID, TextRevisionID, UUID, Text)
        ((DraftRevisionID -> m (Vector CommentAnchor)) -> m DraftRevision)
createDraftTextRevision =
    lmap
        mapInput
        $ rmap
            uncurryDraftRevision
            [singletonStatement|
                WITH inserted AS (
                    INSERT INTO doc_draft_text_revisions
                        (text_element, based_on_revision, author, content)
                    VALUES
                        ($1 :: int8, $2 :: int8, $3 :: uuid, $4 :: text)
                    ON CONFLICT (text_element, author)
                    DO UPDATE SET
                        content = EXCLUDED.content,
                        last_updated_ts = NOW()
                    RETURNING
                        id :: int8,
                        based_on_revision :: int8,
                        creation_ts :: timestamptz,
                        last_updated_ts :: timestamptz,
                        author :: uuid,
                        content :: text
                )
                SELECT
                    inserted.id :: int8,
                    inserted.based_on_revision :: int8,
                    inserted.creation_ts :: timestamptz,
                    inserted.last_updated_ts :: timestamptz,
                    inserted.author :: uuid,
                    users.name :: text,
                    inserted.content :: text
                FROM
                    inserted
                    JOIN users ON users.id = inserted.author
            |]
  where
    mapInput (elementID, revisionID, author, content) =
        (unTextElementID elementID, unTextRevisionID revisionID, author, content)

-- | Get draft revision for a text element by a specific user
getDraftTextRevision
    :: (Monad m)
    => Statement
        (TextElementID, UUID)
        ((DraftRevisionID -> m (Vector CommentAnchor)) -> m (Maybe DraftRevision))
getDraftTextRevision =
    lmap
        (first unTextElementID)
        $ rmap
            (\row f -> mapM (`uncurryDraftRevision` f) row)
            [maybeStatement|
                SELECT
                    drafts.id :: int8,
                    drafts.based_on_revision :: int8,
                    drafts.creation_ts :: timestamptz,
                    drafts.last_updated_ts :: timestamptz,
                    drafts.author :: uuid,
                    users.name :: text,
                    drafts.content :: text
                FROM
                    doc_draft_text_revisions drafts
                    JOIN users ON users.id = drafts.author
                WHERE
                    drafts.text_element = $1 :: int8
                    AND drafts.author = $2 :: uuid
            |]

-- | Delete draft revision
deleteDraftTextRevision :: Statement (TextElementID, UUID) ()
deleteDraftTextRevision =
    lmap
        (first unTextElementID)
        [resultlessStatement|
            DELETE FROM doc_draft_text_revisions
            WHERE
                text_element = $1 :: int8
                AND author = $2 :: uuid
        |]

-- | Get comment anchors for draft revision
getDraftCommentAnchors :: Statement DraftRevisionID (Vector CommentAnchor)
getDraftCommentAnchors =
    lmap
        unDraftRevisionID
        $ rmap
            (fromMaybe Vector.empty . (>>= parseMaybe parseJSON))
            [maybeStatement|
                SELECT
                    comment_anchors :: jsonb
                FROM doc_draft_text_revisions
                WHERE id = $1 :: int8
            |]

-- | Put comment anchors for draft revision (replace all)
putDraftCommentAnchors :: Statement (DraftRevisionID, Vector CommentAnchor) ()
putDraftCommentAnchors =
    lmap
        (bimap unDraftRevisionID (toJSON . Vector.toList))
        [resultlessStatement|
            UPDATE doc_draft_text_revisions
            SET comment_anchors = $2 :: jsonb
            WHERE id = $1 :: int8
        |]
