module Docs
    ( Error (..)
    , Result
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
    ) where

import Control.Monad (unless)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (find)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Vector (Vector)

import UserManagement.DocumentPermission (Permission (..))
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

import Docs.Database
    ( HasCheckDocPermission
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
    , TextElementRevision
    , TextRevisionHistory
    , TextRevisionRef (..)
    , TextRevisionSelector (..)
    , newTextRevision
    )
import Docs.Tree (Node)
import Docs.TreeRevision
    ( TreeRevision
    , TreeRevisionHistory
    , TreeRevisionRef (..)
    )

data Error
    = NoPermission DocumentID Permission
    | NoPermissionInGroup GroupID
    | DocumentNotFound DocumentID
    | TextElementNotFound TextElementRef
    | TextRevisionNotFound TextRevisionRef
    | TreeRevisionNotFound TreeRevisionRef

type Result a = Either Error a

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

getDocuments
    :: (HasGetDocument m)
    => UserID
    -> m (Result (Vector Document))
getDocuments userID = Right <$> DB.getDocuments userID

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
    -> m (Result TextRevisionHistory)
getTextHistory userID ref@(TextElementRef docID _) time = runExceptT $ do
    guardPermission Read docID userID
    guardExistsTextElement ref
    lift $ DB.getTextHistory ref time

getTreeHistory
    :: (HasGetTreeHistory m)
    => UserID
    -> DocumentID
    -> Maybe UTCTime
    -> m (Result TreeRevisionHistory)
getTreeHistory userID docID time = runExceptT $ do
    guardPermission Read docID userID
    guardExistsDocument docID
    lift $ DB.getTreeHistory docID time

getDocumentHistory
    :: (HasGetDocumentHistory m)
    => UserID
    -> DocumentID
    -> Maybe UTCTime
    -> m (Result DocumentHistory)
getDocumentHistory userID docID time = runExceptT $ do
    guardPermission Read docID userID
    guardExistsDocument docID
    lift $ DB.getDocumentHistory docID time

-- guards

guardPermission
    :: (HasCheckDocPermission m)
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
