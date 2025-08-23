module Docs
    ( Error (..)
    , Result
    , Limit
    , createDocument
    , getDocument
    , getDocuments
    , createTextElement
    , createTextRevision
    , getTextElementRevision
    , getTreeRevision
    , createTreeRevision
    , getTextHistory
    , getTreeHistory
    , getDocumentHistory
    , getTreeWithLatestTexts
    , createComment
    , getComments
    , resolveComment
    , createReply
    ) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Data.Vector (Vector)

import UserManagement.DocumentPermission (Permission (..))
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

import Data.Maybe (fromMaybe)
import Docs.Comment (Comment, CommentRef (CommentRef), Message)
import qualified Docs.Comment as Comment
import Docs.Database
    ( HasCheckPermission
    , HasCreateComment
    , HasCreateDocument
    , HasCreateTextElement
    , HasCreateTextRevision
    , HasCreateTreeRevision
    , HasExistsComment
    , HasExistsDocument
    , HasExistsTextElement
    , HasExistsTextRevision
    , HasExistsTreeRevision
    , HasGetComments
    , HasGetDocument
    , HasGetDocumentHistory
    , HasGetTextElementRevision
    , HasGetTextHistory
    , HasGetTreeHistory
    , HasGetTreeRevision
    , HasIsGroupAdmin
    , HasIsSuperAdmin
    )
import qualified Docs.Database as DB
import Docs.Document (Document, DocumentID)
import Docs.DocumentHistory (DocumentHistory)
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementKind
    , TextElementRef (..)
    )
import Docs.TextRevision
    ( ConflictStatus
    , NewTextRevision (..)
    , TextElementRevision (TextElementRevision)
    , TextRevisionHistory
    , TextRevisionRef (..)
    )
import qualified Docs.TextRevision as TextRevision
import Docs.Tree (Node)
import Docs.TreeRevision
    ( TreeRevision
    , TreeRevisionHistory
    , TreeRevisionRef (..)
    )
import qualified Docs.TreeRevision as TreeRevision
import qualified Docs.UserRef as UserRef
import GHC.Int (Int64)

data Error
    = NoPermission DocumentID Permission
    | NoPermissionForUser UserID
    | NoPermissionInGroup GroupID
    | DocumentNotFound DocumentID
    | TextElementNotFound TextElementRef
    | TextRevisionNotFound TextRevisionRef
    | TreeRevisionNotFound TreeRevisionRef
    | CommentNotFound CommentRef

type Result a = Either Error a

type Limit = Int64

defaultHistoryLimit :: Limit
defaultHistoryLimit = 20

squashRevisionsWithinMinutes :: Float
squashRevisionsWithinMinutes = 15

enableSquashing :: Bool
enableSquashing = False

createDocument
    :: (HasCreateDocument m)
    => UserID
    -> GroupID
    -> Text
    -> m (Result Document)
createDocument userID groupID title = runExceptT $ do
    guardGroupAdmin groupID userID
    lift $ DB.createDocument title groupID userID

getDocument
    :: (HasGetDocument m)
    => UserID
    -> DocumentID
    -> m (Result Document)
getDocument userID docID = runExceptT $ do
    guardPermission Read docID userID
    document <- lift $ DB.getDocument docID
    maybe (throwError $ DocumentNotFound docID) pure document

-- | Gets all documents visible by the user
--   OR all documents by the specified group and / or user
getDocuments
    :: (HasGetDocument m)
    => UserID
    -> Maybe UserID
    -> Maybe GroupID
    -> m (Result (Vector Document))
getDocuments userID byUserID byGroupID = case (byUserID, byGroupID) of
    (Nothing, Nothing) -> Right <$> DB.getDocuments userID
    _ -> runExceptT $ do
        maybe (pure ()) (guardUserRights userID) byUserID
        maybe (pure ()) (`guardGroupAdmin` userID) byGroupID
        lift $ DB.getDocumentsBy byUserID byGroupID

createTextElement
    :: (HasCreateTextElement m)
    => UserID
    -> DocumentID
    -> TextElementKind
    -> m (Result TextElement)
createTextElement userID docID kind = runExceptT $ do
    guardPermission Edit docID userID
    guardExistsDocument docID
    lift $ DB.createTextElement docID kind

-- | Create a new 'TextRevision' in the Database.
--
--   Updates the latest revision instead of creating a new one, if
--      - the latest revision is created by the same author,
--      - the latest revision is no older than a set threshold.
--   In case of an update, the revision id is increased nevertheless to
--   prevent lost update scenarios.
createTextRevision
    :: (HasCreateTextRevision m, HasGetTextElementRevision m, HasExistsComment m)
    => UserID
    -> NewTextRevision
    -> m (Result ConflictStatus)
createTextRevision userID revision = runExceptT $ do
    let ref@(TextElementRef docID _) = newTextRevisionElement revision
    guardPermission Edit docID userID
    guardExistsTextElement ref
    mapM_
        guardExistsComment
        (CommentRef ref . Comment.comment <$> newTextRevisionCommentAnchors revision)
    let latestRevisionRef = TextRevisionRef ref TextRevision.Latest
    latestElementRevision <-
        lift $ DB.getTextElementRevision latestRevisionRef
    let latestRevision = latestElementRevision >>= TextRevision.revision
    let latestRevisionID =
            latestRevision
                <&> TextRevision.identifier . TextRevision.header
    let parentRevisionID = newTextRevisionParent revision
    let createRevision =
            DB.createTextRevision
                userID
                ref
                (newTextRevisionContent revision)
                (newTextRevisionCommentAnchors revision)
    lift $ do
        now <- DB.now
        case latestRevision of
            -- first revision
            Nothing -> createRevision <&> TextRevision.NoConflict
            Just latest
                -- content has not changed? -> return latest
                | content latest == newTextRevisionContent revision ->
                    return $ TextRevision.NoConflict latest
                -- no conflict, and can update? -> update (squash)
                | latestRevisionID == parentRevisionID && shouldUpdate now latest ->
                    DB.updateTextRevision
                        (identifier latest)
                        (newTextRevisionContent revision)
                        (newTextRevisionCommentAnchors revision)
                        <&> TextRevision.NoConflict
                -- no conflict, but can not update? -> create new
                | latestRevisionID == parentRevisionID ->
                    createRevision <&> TextRevision.NoConflict
                -- conflict
                | otherwise ->
                    return $
                        TextRevision.Conflict $
                            identifier latest
  where
    header = TextRevision.header
    identifier = TextRevision.identifier . header
    content = TextRevision.content
    timestamp = TextRevision.timestamp . header
    author = TextRevision.author . header
    authorID = UserRef.identifier . author
    shouldUpdate tz latestRevision =
        enableSquashing
            && userID == authorID latestRevision
            && diff < squashRevisionsWithinMinutes
      where
        diff =
            ((/ 60) . realToFrac)
                . diffUTCTime tz
                $ timestamp latestRevision

getTextElementRevision
    :: (HasGetTextElementRevision m)
    => UserID
    -> TextRevisionRef
    -> m (Result (Maybe TextElementRevision))
getTextElementRevision userID ref = runExceptT $ do
    let (TextRevisionRef (TextElementRef docID _) _) = ref
    guardPermission Read docID userID
    guardExistsTextRevision True ref
    lift $ DB.getTextElementRevision ref

createTreeRevision
    :: (HasCreateTreeRevision m)
    => UserID
    -> DocumentID
    -> Node TextElementID
    -> m (Result (TreeRevision TextElementID))
createTreeRevision userID docID root = runExceptT $ do
    guardPermission Edit docID userID
    guardExistsDocument docID
    existsTextElement <- lift $ DB.existsTextElementInDocument docID
    case firstFalse existsTextElement root of
        Just textID -> throwError $ TextElementNotFound $ TextElementRef docID textID
        Nothing -> lift $ DB.createTreeRevision userID docID root
  where
    firstFalse predicate = find (not . predicate)

getTreeRevision
    :: (HasGetTreeRevision m)
    => UserID
    -> TreeRevisionRef
    -> m (Result (Maybe (TreeRevision TextElement)))
getTreeRevision userID ref@(TreeRevisionRef docID _) = runExceptT $ do
    guardPermission Read docID userID
    guardExistsTreeRevision True ref
    lift $ DB.getTreeRevision ref

getTextHistory
    :: (HasGetTextHistory m)
    => UserID
    -> TextElementRef
    -> Maybe UTCTime
    -> Maybe Limit
    -> m (Result TextRevisionHistory)
getTextHistory userID ref@(TextElementRef docID _) time limit = runExceptT $ do
    guardPermission Read docID userID
    guardExistsTextElement ref
    lift $ DB.getTextHistory ref time $ fromMaybe defaultHistoryLimit limit

getTreeHistory
    :: (HasGetTreeHistory m)
    => UserID
    -> DocumentID
    -> Maybe UTCTime
    -> Maybe Limit
    -> m (Result TreeRevisionHistory)
getTreeHistory userID docID time limit = runExceptT $ do
    guardPermission Read docID userID
    guardExistsDocument docID
    lift $ DB.getTreeHistory docID time $ fromMaybe defaultHistoryLimit limit

getDocumentHistory
    :: (HasGetDocumentHistory m)
    => UserID
    -> DocumentID
    -> Maybe UTCTime
    -> Maybe Limit
    -> m (Result DocumentHistory)
getDocumentHistory userID docID time limit = runExceptT $ do
    guardPermission Read docID userID
    guardExistsDocument docID
    lift $ DB.getDocumentHistory docID time $ fromMaybe defaultHistoryLimit limit

getTreeWithLatestTexts
    :: (HasGetTreeRevision m, HasGetTextElementRevision m)
    => UserID
    -> TreeRevisionRef
    -> m (Result (Maybe (TreeRevision TextElementRevision)))
getTreeWithLatestTexts userID revision = runExceptT $ do
    guardPermission Read docID userID
    guardExistsDocument docID
    guardExistsTreeRevision True revision
    lift $ do
        treeRevision <- DB.getTreeRevision revision
        mapM (TreeRevision.withTextRevisions getter') treeRevision
  where
    (TreeRevisionRef docID _) = revision
    getter =
        DB.getTextElementRevision
            . (`TextRevisionRef` TextRevision.Latest)
            . TextElementRef docID
    getter' = (<&> (>>= elementRevisionToRevision)) . getter
    elementRevisionToRevision (TextElementRevision _ rev) = rev

createComment
    :: (HasCreateComment m)
    => UserID
    -> TextElementRef
    -> Text
    -> m (Result Comment)
createComment userID ref@(TextElementRef docID textID) text = runExceptT $ do
    guardPermission Comment docID userID
    guardExistsTextElement ref
    lift $ DB.createComment userID textID text

getComments
    :: (HasGetComments m)
    => UserID
    -> TextElementRef
    -> m (Result (Vector Comment))
getComments userID ref@(TextElementRef docID _) = runExceptT $ do
    guardPermission Read docID userID
    guardExistsTextElement ref
    lift $ DB.getComments ref

resolveComment
    :: (HasCreateComment m)
    => UserID
    -> CommentRef
    -> m (Result ())
resolveComment userID ref@(CommentRef (TextElementRef docID _) commentID) = runExceptT $ do
    guardPermission Comment docID userID
    guardExistsComment ref
    lift $ DB.resolveComment commentID

createReply
    :: (HasCreateComment m)
    => UserID
    -> CommentRef
    -> Text
    -> m (Result Message)
createReply userID ref@(CommentRef (TextElementRef docID _) commentID) content = runExceptT $ do
    guardPermission Comment docID userID
    guardExistsComment ref
    lift $ DB.createReply userID commentID content

-- guards

guardPermission
    :: (HasCheckPermission m)
    => Permission
    -> DocumentID
    -> UserID
    -> ExceptT Error m ()
guardPermission perms docID userID = do
    hasPermission <- lift $ DB.checkDocumentPermission userID docID perms
    superAdmin <- lift $ DB.isSuperAdmin userID
    unless (hasPermission || superAdmin) $
        throwError (NoPermission docID perms)

guardGroupAdmin
    :: (HasIsGroupAdmin m)
    => GroupID
    -> UserID
    -> ExceptT Error m ()
guardGroupAdmin groupID userID = do
    hasPermission <- lift $ DB.isGroupAdmin userID groupID
    superAdmin <- lift $ DB.isSuperAdmin userID
    unless (hasPermission || superAdmin) $
        throwError (NoPermissionInGroup groupID)

guardUserRights
    :: (HasIsSuperAdmin m)
    => UserID
    -> UserID
    -> ExceptT Error m ()
guardUserRights userID forUserID = do
    superAdmin <- lift $ DB.isSuperAdmin userID
    unless (userID == forUserID || superAdmin) $
        throwError (NoPermissionForUser forUserID)

guardExistsDocument
    :: (HasExistsDocument m)
    => DocumentID
    -> ExceptT Error m ()
guardExistsDocument docID = do
    existsDocument <- lift $ DB.existsDocument docID
    unless existsDocument $
        throwError (DocumentNotFound docID)

guardExistsTreeRevision
    :: (HasExistsTreeRevision m)
    => Bool
    -- ^ wether or not to consider `Latest` to exist if no revision exists.
    -> TreeRevisionRef
    -- ^ reference to the revision
    -> ExceptT Error m ()
guardExistsTreeRevision allowLatestNothing ref@(TreeRevisionRef docID selector) = do
    guardExistsDocument docID
    existsTreeRevision <- lift $ DB.existsTreeRevision ref
    let considerExistant = case selector of
            TreeRevision.Latest -> existsTreeRevision || allowLatestNothing
            TreeRevision.Specific _ -> existsTreeRevision
    unless considerExistant $
        throwError (TreeRevisionNotFound ref)

guardExistsTextElement
    :: (HasExistsTextElement m)
    => TextElementRef
    -> ExceptT Error m ()
guardExistsTextElement ref@(TextElementRef docID _) = do
    guardExistsDocument docID
    existsTextElement <- lift $ DB.existsTextElement ref
    unless existsTextElement $
        throwError (TextElementNotFound ref)

guardExistsTextRevision
    :: (HasExistsTextRevision m)
    => Bool
    -- ^ wether or not to consider `Latest` to exist if no revision exists.
    -> TextRevisionRef
    -- ^ reference to the revision
    -> ExceptT Error m ()
guardExistsTextRevision allowLatestNothing ref@(TextRevisionRef elementRef selector) = do
    guardExistsTextElement elementRef
    existsTextRevision <- lift $ DB.existsTextRevision ref
    let considerExistant = case selector of
            TextRevision.Latest -> existsTextRevision || allowLatestNothing
            TextRevision.Specific _ -> existsTextRevision
    unless considerExistant $
        throwError (TextRevisionNotFound ref)

guardExistsComment
    :: (HasExistsComment m)
    => CommentRef
    -> ExceptT Error m ()
guardExistsComment ref@(CommentRef textRef _) = do
    guardExistsTextElement textRef
    existsComment <- lift $ DB.existsComment ref
    unless existsComment $
        throwError (CommentNotFound ref)
