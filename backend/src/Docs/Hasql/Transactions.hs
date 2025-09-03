{-# LANGUAGE TupleSections #-}

module Docs.Hasql.Transactions
    ( now
    , createDocument
    , createTextElement
    , getTextElementRevision
    , existsTextRevision
    , updateTextRevision
    , createTextRevision
    , putTree
    , createTreeRevision
    , existsDocument
    , existsTextElement
    , getLatestTextRevisionID
    , isTextElementInDocument
    , hasPermission
    , isGroupAdmin
    , createComment
    , existsComment
    , resolveComment
    , createReply
    , logMessage
    , updateLatestTitle
    , getTextElement
    , createDraftTextRevision
    , getDraftTextRevision
    , deleteDraftTextRevision
    ) where

import qualified Crypto.Hash.SHA1 as SHA1
import Hasql.Transaction (Transaction, statement)

import Control.Monad (guard)
import Data.Functor ((<&>))
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import UserManagement.DocumentPermission (Permission)
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

import Data.Aeson (ToJSON)
import Docs.Comment (Comment, CommentAnchor, CommentID, CommentRef, Message)
import qualified Docs.Comment as Comment
import Docs.Document (Document, DocumentID)
import Docs.Hash
    ( Hash (Hash)
    , Hashable (..)
    )
import qualified Docs.Hasql.Statements as Statements
import Docs.Hasql.TreeEdge (TreeEdge (TreeEdge), TreeEdgeChildRef (..))
import qualified Docs.Hasql.TreeEdge as TreeEdge
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementKind
    , TextElementRef (..)
    )
import Docs.TextRevision
    ( DraftRevision
    , TextElementRevision
    , TextRevision
    , TextRevisionID
    , TextRevisionRef
    )
import Docs.Tree (Edge (Edge), Node (Node), Tree (Leaf, Tree))
import Docs.TreeRevision (TreeRevision, TreeRevisionRef (TreeRevisionRef))
import qualified Docs.TreeRevision as TreeRevision
import Logging.Logs (LogMessage, Scope, Severity)

now :: Transaction UTCTime
now = statement () Statements.now

getTextElementRevision
    :: TextRevisionRef
    -> Transaction (Maybe TextElementRevision)
getTextElementRevision ref = do
    textElementRevision <- statement ref Statements.getTextElementRevision
    textElementRevision $ flip statement Statements.getCommentAnchors

createDocument :: Text -> GroupID -> UserID -> Transaction Document
createDocument name group user =
    statement (name, group, user) Statements.createDocument

createTextElement :: DocumentID -> TextElementKind -> Transaction TextElement
createTextElement = curry (`statement` Statements.createTextElement)

existsTextRevision :: TextRevisionRef -> Transaction Bool
existsTextRevision = flip statement Statements.existsTextRevision

existsDocument :: DocumentID -> Transaction Bool
existsDocument = flip statement Statements.existsDocument

existsTextElement :: TextElementRef -> Transaction Bool
existsTextElement = flip statement Statements.existsTextElement

getLatestTextRevisionID :: TextElementRef -> Transaction (Maybe TextRevisionID)
getLatestTextRevisionID = (`statement` Statements.getLatestTextRevisionID)

updateTextRevision
    :: TextRevisionID
    -> Text
    -> Vector CommentAnchor
    -> Transaction TextRevision
updateTextRevision rev text commentAnchors = do
    statement
        (rev, Comment.comment <$> commentAnchors)
        Statements.deleteCommentAnchorsExcept
    textRevision <- statement (rev, text) Statements.updateTextRevision
    textRevision $
        const $
            mapM (`statement` Statements.putCommentAnchor) ((rev,) <$> commentAnchors)

createTextRevision
    :: UserID
    -> TextElementRef
    -> Text
    -> Vector CommentAnchor
    -> Transaction TextRevision
createTextRevision userID (TextElementRef _ textID) content commentAnchors = do
    textRevision <-
        statement (textID, userID, content) Statements.createTextRevision
    textRevision $
        \rev ->
            mapM (`statement` Statements.putCommentAnchor) ((rev,) <$> commentAnchors)

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
    current <-
        statement
            (TreeRevisionRef docID TreeRevision.Latest)
            Statements.getTreeRevision
    let keepCurrent = current >>= guard . ((== rootHash) . fst) >> current <&> snd
    case keepCurrent of
        Just currentRevision ->
            return $ currentRevision rootNode
        Nothing -> do
            revision <-
                statement
                    (docID, authorID, rootHash)
                    Statements.putTreeRevision
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
    statement (userID, docID, perms) Statements.hasPermission

isGroupAdmin :: UserID -> GroupID -> Transaction Bool
isGroupAdmin userID groupID =
    statement (userID, groupID) Statements.isGroupAdmin

createComment :: UserID -> TextElementID -> Text -> Transaction Comment
createComment userID textElemID text =
    statement (userID, textElemID, text) Statements.createComment

existsComment :: CommentRef -> Transaction Bool
existsComment =
    flip statement Statements.existsComment

resolveComment :: CommentID -> Transaction ()
resolveComment =
    flip statement Statements.resolveComment

createReply :: UserID -> CommentID -> Text -> Transaction Message
createReply userID commentID = (`statement` Statements.createReply) . (userID,commentID,)

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
    -> Transaction LogMessage
    -- ^ created log message
logMessage severity source scope content =
    statement (severity, source, scope, content) Statements.logMessage

updateLatestTitle :: TextElementID -> Text -> Transaction ()
updateLatestTitle = curry (`statement` Statements.updateLatestTitle)

getTextElement :: TextElementID -> Transaction (Maybe TextElement)
getTextElement = flip statement Statements.getTextElement

-- | Create or update a draft text revision for a user
createDraftTextRevision
    :: UserID
    -> TextElementRef
    -> TextRevisionID
    -> Text
    -> Vector CommentAnchor
    -> Transaction DraftRevision
createDraftTextRevision userID (TextElementRef _ textID) basedOnRevision content commentAnchors = do
    draftRevision <-
        statement
            (textID, basedOnRevision, userID, content)
            Statements.createDraftTextRevision
    draftRevision $ \draftId ->
        mapM_ (`statement` Statements.putDraftCommentAnchors) [(draftId, commentAnchors)]
            >> statement draftId Statements.getDraftCommentAnchors

-- | Get draft revision for a text element by a specific user
getDraftTextRevision
    :: UserID -> TextElementRef -> Transaction (Maybe DraftRevision)
getDraftTextRevision userID (TextElementRef _ textID) = do
    draftGetter <- statement (textID, userID) Statements.getDraftTextRevision
    draftGetter (`statement` Statements.getDraftCommentAnchors)

-- | Delete draft revision for a text element by a user
deleteDraftTextRevision :: UserID -> TextElementRef -> Transaction ()
deleteDraftTextRevision userID (TextElementRef _ textID) =
    statement (textID, userID) Statements.deleteDraftTextRevision
