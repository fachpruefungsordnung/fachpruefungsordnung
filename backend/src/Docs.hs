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
    ) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)

import UserManagement.DocumentPermission (Permission (..))
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

import Data.Maybe (fromMaybe)
import Docs.Database
    ( HasCheckPermission
    , HasCreateDocument
    , HasCreateTextElement
    , HasCreateTextRevision
    , HasCreateTreeRevision
    , HasExistsDocument
    , HasExistsTextElement
    , HasExistsTextRevision
    , HasExistsTreeRevision
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
    , TextRevisionSelector (..)
    , newTextRevision
    )
import qualified Docs.TextRevision as TextRevision
import Docs.Tree (Node)
import Docs.TreeRevision
    ( TreeRevision
    , TreeRevisionHistory
    , TreeRevisionRef (..)
    )
import qualified Docs.TreeRevision as TreeRevision
import GHC.Int (Int32)

data Error
    = NoPermission DocumentID Permission
    | NoPermissionForUser UserID
    | NoPermissionInGroup GroupID
    | DocumentNotFound DocumentID
    | TextElementNotFound TextElementRef
    | TextRevisionNotFound TextRevisionRef
    | TreeRevisionNotFound TreeRevisionRef

type Result a = Either Error a

type Limit = Int32

defaultHistoryLimit :: Limit
defaultHistoryLimit = 20

createDocument
    :: (HasCreateDocument m)
    => UserID
    -> GroupID
    -> Text
    -> m (Result Document)
createDocument userID groupID title = runExceptT $ do
    guardGroupAdmin groupID userID
    lift $ DB.createDocument title groupID

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

createTextRevision
    :: (HasCreateTextRevision m)
    => UserID
    -> NewTextRevision
    -> m (Result ConflictStatus)
createTextRevision userID revision = runExceptT $ do
    let ref@(TextElementRef docID _) = newTextRevisionElement revision
    guardPermission Edit docID userID
    guardExistsTextElement ref
    lift $
        newTextRevision
            DB.getLatestTextRevisionID
            DB.createTextRevision
            userID
            revision

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
    -> m (Result (TreeRevision TextElement))
getTreeRevision userID ref@(TreeRevisionRef docID _) = runExceptT $ do
    guardPermission Read docID userID
    guardExistsTreeRevision ref
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
    -> m (Result (TreeRevision TextElementRevision))
getTreeWithLatestTexts userID revision = runExceptT $ do
    guardPermission Read docID userID
    guardExistsDocument docID
    guardExistsTreeRevision revision
    lift $
        DB.getTreeRevision revision
            >>= TreeRevision.withTextRevisions getter'
  where
    (TreeRevisionRef docID _) = revision
    getter =
        DB.getTextElementRevision
            . (`TextRevisionRef` TextRevision.Latest)
            . TextElementRef docID
    getter' = (<&> (>>= elementRevisionToRevision)) . getter
    elementRevisionToRevision (TextElementRevision _ rev) = rev

-- guards

guardPermission
    :: (HasCheckPermission m)
    => Permission
    -> DocumentID
    -> UserID
    -> ExceptT Error m ()
guardPermission perms docID userID = do
    hasPermission <- lift $ DB.checkDocumentPermission userID docID perms
    unless hasPermission $
        throwError (NoPermission docID perms)

guardGroupAdmin
    :: (HasIsGroupAdmin m)
    => GroupID
    -> UserID
    -> ExceptT Error m ()
guardGroupAdmin groupID userID = do
    hasPermission <- lift $ DB.isGroupAdmin userID groupID
    unless hasPermission $
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
    => TreeRevisionRef
    -> ExceptT Error m ()
guardExistsTreeRevision ref@(TreeRevisionRef docID _) = do
    guardExistsDocument docID
    existsTreeRevision <- lift $ DB.existsTreeRevision ref
    unless existsTreeRevision $
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
            Latest -> existsTextRevision || allowLatestNothing
            Specific _ -> existsTextRevision
    unless considerExistant $
        throwError (TextRevisionNotFound ref)
