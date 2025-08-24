module Docs.Hasql.Sessions
    ( createDocument
    , getDocument
    , getDocuments
    , getDocumentsBy
    , createTextElement
    , createTextRevision
    , getTextElementRevision
    , createTreeRevision
    , getTreeRevision
    , getTree
    , getTextRevisionHistory
    , getTreeRevisionHistory
    , getDocumentRevisionHistory
    , existsDocument
    , existsTextElement
    , existsTextRevision
    , existsTreeRevision
    , hasPermission
    , isGroupAdmin
    , getComments
    , logMessage
    , getLogs
    ) where

import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Hasql.Session (Session, statement)
import Hasql.Transaction.Sessions
    ( IsolationLevel (..)
    , Mode (..)
    , transaction
    )

import UserManagement.DocumentPermission (Permission)
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

import Data.Aeson (ToJSON)
import Docs.Comment (Comment, CommentAnchor, CommentRef (CommentRef), Message)
import qualified Docs.Comment as Comment
import Docs.Document (Document, DocumentID)
import Docs.DocumentHistory (DocumentHistory (..))
import Docs.Hash (Hash)
import qualified Docs.Hasql.Statements as Statements
import qualified Docs.Hasql.Transactions as Transactions
import Docs.Hasql.TreeEdge (TreeEdgeChild (..))
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementKind
    , TextElementRef (TextElementRef)
    )
import Docs.TextRevision
    ( TextElementRevision
    , TextRevision
    , TextRevisionHistory (TextRevisionHistory)
    , TextRevisionRef
    )
import Docs.Tree (Edge (Edge), Node (Node), NodeHeader)
import qualified Docs.Tree as Tree
import Docs.TreeRevision
    ( TreeRevision
    , TreeRevisionHistory (TreeRevisionHistory)
    , TreeRevisionRef (..)
    )
import GHC.Int (Int64)
import Logging.Logs (LogMessage, Scope, Severity)

existsDocument :: DocumentID -> Session Bool
existsDocument = flip statement Statements.existsDocument

existsTextElement :: TextElementRef -> Session Bool
existsTextElement = flip statement Statements.existsTextElement

existsTextRevision :: TextRevisionRef -> Session Bool
existsTextRevision = flip statement Statements.existsTextRevision

existsTreeRevision :: TreeRevisionRef -> Session Bool
existsTreeRevision = flip statement Statements.existsTreeRevision

createDocument :: Text -> GroupID -> UserID -> Session Document
createDocument name group user =
    statement (name, group, user) Statements.createDocument

getDocument :: DocumentID -> Session (Maybe Document)
getDocument = (`statement` Statements.getDocument)

getDocuments :: UserID -> Session (Vector Document)
getDocuments = (`statement` Statements.getDocuments)

getDocumentsBy :: Maybe UserID -> Maybe GroupID -> Session (Vector Document)
getDocumentsBy = curry (`statement` Statements.getDocumentsBy)

createTextElement :: DocumentID -> TextElementKind -> Session TextElement
createTextElement = curry (`statement` Statements.createTextElement)

createTextRevision
    :: UserID
    -> TextElementRef
    -> Text
    -> Vector CommentAnchor
    -> Session TextRevision
createTextRevision =
    (((transaction Serializable Write .) .) .) . Transactions.createTextRevision

getTextElementRevision
    :: TextRevisionRef
    -> Session (Maybe TextElementRevision)
getTextElementRevision ref = do
    textElementRevision <- statement ref Statements.getTextElementRevision
    textElementRevision $ flip statement Statements.getCommentAnchors

createTreeRevision
    :: UserID
    -> DocumentID
    -> Node TextElementID
    -> Session (TreeRevision TextElementID)
createTreeRevision authorID docID rootNode =
    transaction
        Serializable
        Write
        $ Transactions.createTreeRevision authorID docID rootNode

getTreeRevision
    :: TreeRevisionRef
    -> Session (Maybe (TreeRevision TextElement))
getTreeRevision ref = do
    revision <- getRevision
    case revision of
        Just (rootHash, treeRevision) -> do
            root <- getTree rootHash
            return $ Just $ treeRevision root
        Nothing -> return Nothing
  where
    getRevision = statement ref Statements.getTreeRevision

getTree :: Hash -> Session (Node TextElement)
getTree rootHash = do
    rootHeader <- statement rootHash Statements.getTreeNode
    fromHeader rootHash rootHeader
  where
    fromHeader :: Hash -> NodeHeader -> Session (Node TextElement)
    fromHeader hash header = do
        children <- statement hash Statements.getTreeEdgesByParent
        edges <- mapM edgeSelector children
        return $ Node header $ Vector.toList edges
    edgeSelector :: (Text, TreeEdgeChild) -> Session (Edge TextElement)
    edgeSelector (title, edge) =
        Edge title <$> case edge of
            (TreeEdgeToTextElement textElement) -> return $ Tree.Leaf textElement
            (TreeEdgeToNode hash header) -> fromHeader hash header <&> Tree.Tree

getTextRevisionHistory
    :: TextElementRef -> Maybe UTCTime -> Int64 -> Session TextRevisionHistory
getTextRevisionHistory ref before limit =
    statement (ref, before, limit) Statements.getTextRevisionHistory
        <&> TextRevisionHistory ref . Vector.toList

getTreeRevisionHistory
    :: DocumentID -> Maybe UTCTime -> Int64 -> Session TreeRevisionHistory
getTreeRevisionHistory id_ before limit =
    statement (id_, before, limit) Statements.getTreeRevisionHistory
        <&> TreeRevisionHistory id_ . Vector.toList

getDocumentRevisionHistory
    :: DocumentID -> Maybe UTCTime -> Int64 -> Session DocumentHistory
getDocumentRevisionHistory id_ before limit =
    statement (id_, before, limit) Statements.getDocumentRevisionHistory
        <&> DocumentHistory id_ . Vector.toList

hasPermission :: UserID -> DocumentID -> Permission -> Session Bool
hasPermission userID docID perms =
    statement (userID, docID, perms) Statements.hasPermission

isGroupAdmin :: UserID -> GroupID -> Session Bool
isGroupAdmin userID groupID =
    statement (userID, groupID) Statements.isGroupAdmin

getComments :: TextElementRef -> Session (Vector Comment)
getComments textRef@(TextElementRef docID textID) = do
    comments <- statement (docID, textID) Statements.getComments
    mapM mapper comments
  where
    mapper :: Comment -> Session Comment
    mapper comment = do
        replies <- getReplies $ CommentRef textRef $ Comment.identifier comment
        return $ comment {Comment.replies = replies}
    getReplies :: CommentRef -> Session (Vector Message)
    getReplies = flip statement Statements.getReplies

logMessage
    :: (ToJSON v)
    => Severity
    -- ^ severity of the log message
    -> Maybe UserID
    -- ^ source user
    -> Scope
    -- ^ scope (e.g, "docs.text.revision")
    -> v
    -- ^ content (json)
    -> Session LogMessage
    -- ^ created log message
logMessage severity source scope content =
    statement (severity, source, scope, content) Statements.logMessage

getLogs
    :: Maybe UTCTime
    -- ^ offset
    -> Int64
    -- ^ limit
    -> Session (Vector LogMessage)
    -- ^ log messages
getLogs = curry (`statement` Statements.getLogs)
