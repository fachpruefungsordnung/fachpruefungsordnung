{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Docs
    ( Error (..)
    , Result
    , Limit
    , logMessage
    , newDefaultDocument
    , createDocument
    , getDocument
    , getDocuments
    , createTextElement
    , createTextRevision
    , getTextElementRevision
    , getDocumentRevisionText
    , getTreeRevision
    , getDocumentRevisionTree
    , createTreeRevision
    , getTextHistory
    , getTreeHistory
    , getDocumentHistory
    , getTreeWithLatestTexts
    , getDocumentRevision
    , createComment
    , getComments
    , resolveComment
    , createReply
    , getLogs
    ) where

import Control.Monad (join, msum, unless)
import Control.Monad.Except (ExceptT (ExceptT), runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (find)
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Time (UTCTime, diffUTCTime)
import Data.Vector (Vector)

import UserManagement.DocumentPermission (Permission (..))
import UserManagement.Group (GroupID)
import UserManagement.User (UserID)

import qualified Language.Lsd.AST.Common as LSD
import qualified Language.Ltml.Common as LTML
import qualified Language.Ltml.Tree as LTML

import Control.Applicative ((<|>))
import Data.Aeson (FromJSON, ToJSON)
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
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
    , HasGetLogs
    , HasGetRevisionKey
    , HasGetTextElementRevision
    , HasGetTextHistory
    , HasGetTreeHistory
    , HasGetTreeRevision
    , HasIsGroupAdmin
    , HasIsSuperAdmin
    , HasLogMessage
    )
import qualified Docs.Database as DB
import Docs.Document (Document, DocumentID)
import qualified Docs.Document as Document
import Docs.DocumentHistory (DocumentHistory)
import Docs.FullDocument (FullDocument (FullDocument))
import qualified Docs.FullDocument as FullDocument
import Docs.Revision
    ( RevisionRef (RevisionRef)
    , textRevisionRefFor
    , treeRevisionRefFor
    )
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementKind
    , TextElementRef (..)
    )
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( ConflictStatus
    , NewTextRevision (..)
    , TextElementRevision (TextElementRevision)
    , TextRevisionHistory
    , TextRevisionRef (..)
    )
import qualified Docs.TextRevision as TextRevision
import Docs.Tree (Edge (Edge), Node (Node), Tree)
import qualified Docs.Tree as Tree
import Docs.TreeRevision
    ( TreeRevision
    , TreeRevisionHistory
    , TreeRevisionRef (..)
    )
import qualified Docs.TreeRevision as TreeRevision
import qualified Docs.UserRef as UserRef
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Logging.Logs (LogMessage, Severity (Warning))
import Logging.Scope (Scope)
import qualified Logging.Scope as Scope

data Error
    = NoPermission DocumentID Permission
    | NoPermissionForUser UserID
    | NoPermissionInGroup GroupID
    | SuperAdminOnly
    | DocumentNotFound DocumentID
    | RevisionNotFound RevisionRef
    | TextElementNotFound TextElementRef
    | TextRevisionNotFound TextRevisionRef
    | TreeRevisionNotFound TreeRevisionRef
    | CommentNotFound CommentRef
    | Custom Text
    deriving (Generic)

instance ToJSON Error

instance FromJSON Error

instance ToSchema Error

type Result a = Either Error a

type Limit = Int64

defaultHistoryLimit :: Limit
defaultHistoryLimit = 20

squashRevisionsWithinMinutes :: Float
squashRevisionsWithinMinutes = 15

enableSquashing :: Bool
enableSquashing = False

logged :: (HasLogMessage m) => UserID -> Scope -> m (Result a) -> m (Result a)
logged userID scope result = do
    value <- result
    case value of
        Left err -> do
            _ <- DB.logMessage Warning (Just userID) scope err
            --                 ^~~~~~~ all of these errors are user errors
            return $ Left err
        Right val -> return $ Right val

logMessage
    :: (HasLogMessage m, ToJSON v)
    => Severity
    -> Maybe UserID
    -> Scope
    -> v
    -> m LogMessage
logMessage = DB.logMessage

getLogs
    :: (HasGetLogs m, HasLogMessage m)
    => UserID
    -> Maybe UTCTime
    -> Int64
    -> m (Result (Vector LogMessage))
getLogs userID offset limit = logged userID Scope.logging $ runExceptT $ do
    guardSuperAdmin userID
    lift $ DB.getLogs offset limit

createDocument
    :: (HasCreateDocument m, HasLogMessage m)
    => UserID
    -> GroupID
    -> Text
    -> m (Result Document)
createDocument userID groupID title = logged userID Scope.docs $ runExceptT $ do
    guardGroupAdmin groupID userID
    lift $ DB.createDocument title groupID userID

getDocument
    :: (HasGetDocument m, HasLogMessage m)
    => UserID
    -> DocumentID
    -> m (Result Document)
getDocument userID docID = logged userID Scope.docs $ runExceptT $ do
    guardPermission Read docID userID
    document <- lift $ DB.getDocument docID
    maybe (throwError $ DocumentNotFound docID) pure document

-- | Gets all documents visible by the user
--   OR all documents by the specified group and / or user
getDocuments
    :: (HasGetDocument m, HasLogMessage m)
    => UserID
    -> Maybe UserID
    -> Maybe GroupID
    -> m (Result (Vector Document))
getDocuments userID byUserID byGroupID = logged userID Scope.docs $
    case (byUserID, byGroupID) of
        (Nothing, Nothing) -> Right <$> DB.getDocuments userID
        _ -> runExceptT $ do
            maybe (pure ()) (guardUserRights userID) byUserID
            maybe (pure ()) (`guardGroupAdmin` userID) byGroupID
            lift $ DB.getDocumentsBy byUserID byGroupID

createTextElement
    :: (HasCreateTextElement m, HasLogMessage m)
    => UserID
    -> DocumentID
    -> TextElementKind
    -> m (Result TextElement)
createTextElement userID docID kind = logged userID Scope.docsText $ runExceptT $ do
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
    :: ( HasCreateTextRevision m
       , HasGetTextElementRevision m
       , HasExistsComment m
       , HasLogMessage m
       )
    => UserID
    -> NewTextRevision
    -> m (Result ConflictStatus)
createTextRevision userID revision = logged userID Scope.docsTextRevision $
    runExceptT $ do
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
                    | TextRevision.contentsNotChanged latest revision ->
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
    :: (HasGetTextElementRevision m, HasLogMessage m)
    => UserID
    -> TextRevisionRef
    -> m (Result (Maybe TextElementRevision))
getTextElementRevision userID ref = logged userID Scope.docsTextRevision $
    runExceptT $ do
        let (TextRevisionRef (TextElementRef docID _) _) = ref
        guardPermission Read docID userID
        guardExistsTextRevision True ref
        lift $ DB.getTextElementRevision ref

getDocumentRevisionText
    :: (HasGetTextElementRevision m, HasGetRevisionKey m, HasLogMessage m)
    => UserID
    -> RevisionRef
    -> TextElementID
    -> m (Result (Maybe TextElementRevision))
getDocumentRevisionText userID ref@(RevisionRef docID _) textID =
    logged userID Scope.docsTextRevision $ runExceptT $ do
        guardPermission Read docID userID
        guardExistsDocument docID
        lift $ do
            key <- DB.getRevisionKey ref
            let textRef = TextElementRef docID textID
            result <-
                mapM (DB.getTextElementRevision . textRevisionRefFor textRef) key
            return $ join result

createTreeRevision
    :: (HasCreateTreeRevision m, HasLogMessage m)
    => UserID
    -> DocumentID
    -> Node TextElementID
    -> m (Result (TreeRevision TextElementID))
createTreeRevision userID docID root = logged userID Scope.docsTreeRevision $
    runExceptT $ do
        guardPermission Edit docID userID
        guardExistsDocument docID
        existsTextElement <- lift $ DB.existsTextElementInDocument docID
        case firstFalse existsTextElement root of
            Just textID -> throwError $ TextElementNotFound $ TextElementRef docID textID
            Nothing -> lift $ DB.createTreeRevision userID docID root
  where
    firstFalse predicate = find (not . predicate)

getTreeRevision
    :: (HasGetTreeRevision m, HasLogMessage m)
    => UserID
    -> TreeRevisionRef
    -> m (Result (Maybe (TreeRevision TextElement)))
getTreeRevision userID ref@(TreeRevisionRef docID _) =
    logged userID Scope.docsTreeRevision $
        runExceptT $ do
            guardPermission Read docID userID
            guardExistsTreeRevision True ref
            lift $ DB.getTreeRevision ref

getDocumentRevisionTree
    :: (HasGetTreeRevision m, HasGetRevisionKey m, HasLogMessage m)
    => UserID
    -> RevisionRef
    -> m (Result (Maybe (TreeRevision TextElement)))
getDocumentRevisionTree userID ref@(RevisionRef docID _) =
    logged userID Scope.docsTreeRevision $
        runExceptT $ do
            guardPermission Read docID userID
            guardExistsDocument docID
            lift $ do
                key <- DB.getRevisionKey ref
                result <- mapM (DB.getTreeRevision . treeRevisionRefFor docID) key
                return $ join result

getTextHistory
    :: (HasGetTextHistory m, HasLogMessage m)
    => UserID
    -> TextElementRef
    -> Maybe UTCTime
    -> Maybe Limit
    -> m (Result TextRevisionHistory)
getTextHistory userID ref@(TextElementRef docID _) time limit = logged userID Scope.docsText $
    runExceptT $ do
        guardPermission Read docID userID
        guardExistsTextElement ref
        lift $ DB.getTextHistory ref time $ fromMaybe defaultHistoryLimit limit

getTreeHistory
    :: (HasGetTreeHistory m, HasLogMessage m)
    => UserID
    -> DocumentID
    -> Maybe UTCTime
    -> Maybe Limit
    -> m (Result TreeRevisionHistory)
getTreeHistory userID docID time limit = logged userID Scope.docsTree $
    runExceptT $ do
        guardPermission Read docID userID
        guardExistsDocument docID
        lift $ DB.getTreeHistory docID time $ fromMaybe defaultHistoryLimit limit

getDocumentHistory
    :: (HasGetDocumentHistory m, HasLogMessage m)
    => UserID
    -> DocumentID
    -> Maybe UTCTime
    -> Maybe Limit
    -> m (Result DocumentHistory)
getDocumentHistory userID docID time limit = logged userID Scope.docs $
    runExceptT $ do
        guardPermission Read docID userID
        guardExistsDocument docID
        lift $ DB.getDocumentHistory docID time $ fromMaybe defaultHistoryLimit limit

getTreeWithLatestTexts
    :: (HasGetTreeRevision m, HasGetTextElementRevision m, HasLogMessage m)
    => UserID
    -> TreeRevisionRef
    -> m (Result (Maybe (TreeRevision TextElementRevision)))
getTreeWithLatestTexts userID revision = logged userID Scope.docs $ runExceptT $ do
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

getDocumentRevision
    :: ( HasGetTreeRevision m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , HasLogMessage m
       )
    => UserID
    -> RevisionRef
    -> m (Result FullDocument)
getDocumentRevision userID ref@(RevisionRef docID _) =
    logged userID Scope.docs $ runExceptT $ do
        guardPermission Read docID userID
        guardExistsDocument docID
        maybeDocument <- lift $ DB.getDocument docID
        document <- maybe (throwError $ DocumentNotFound docID) pure maybeDocument
        lift $ do
            key <- DB.getRevisionKey ref
            result <- mapM (DB.getTreeRevision . treeRevisionRefFor docID) key
            let tree = join result
            body <- mapM (TreeRevision.withTextRevisions getter') tree
            return
                FullDocument
                    { FullDocument.header = document
                    , FullDocument.body = body
                    }
  where
    getter textID = do
        key <- DB.getRevisionKey ref
        result <-
            mapM
                ( DB.getTextElementRevision
                    . textRevisionRefFor
                        (TextElementRef docID textID)
                )
                key
        return $ join result
    getter' = (<&> (>>= elementRevisionToRevision)) . getter
    elementRevisionToRevision (TextElementRevision _ rev) = rev

createComment
    :: (HasCreateComment m, HasLogMessage m)
    => UserID
    -> TextElementRef
    -> Text
    -> m (Result Comment)
createComment userID ref@(TextElementRef docID textID) text =
    logged userID Scope.docsComment $ runExceptT $ do
        guardPermission Comment docID userID
        guardExistsTextElement ref
        lift $ DB.createComment userID textID text

getComments
    :: (HasGetComments m, HasLogMessage m)
    => UserID
    -> TextElementRef
    -> m (Result (Vector Comment))
getComments userID ref@(TextElementRef docID _) = logged userID Scope.docsComment $
    runExceptT $ do
        guardPermission Read docID userID
        guardExistsTextElement ref
        lift $ DB.getComments ref

resolveComment
    :: (HasCreateComment m, HasLogMessage m)
    => UserID
    -> CommentRef
    -> m (Result ())
resolveComment userID ref@(CommentRef (TextElementRef docID _) commentID) =
    logged userID Scope.docsComment $ runExceptT $ do
        guardPermission Comment docID userID
        guardExistsComment ref
        lift $ DB.resolveComment commentID

createReply
    :: (HasCreateComment m, HasLogMessage m)
    => UserID
    -> CommentRef
    -> Text
    -> m (Result Message)
createReply userID ref@(CommentRef (TextElementRef docID _) commentID) content =
    logged userID Scope.docsComment $ runExceptT $ do
        guardPermission Comment docID userID
        guardExistsComment ref
        lift $ DB.createReply userID commentID content

newDefaultDocument
    :: ( HasCreateDocument m
       , HasLogMessage m
       , HasCreateTextElement m
       , HasCreateTextRevision m
       , HasGetTextElementRevision m
       , HasExistsComment m
       , HasCreateTreeRevision m
       )
    => UserID
    -> GroupID
    -> Text
    -> LTML.FlaggedInputTree'
    -> m (Result FullDocument)
newDefaultDocument userID groupID title tree = runExceptT $ do
    doc <- ExceptT $ createDocument userID groupID title
    let docID = Document.identifier doc
    let emplaceTexts (LTML.Flagged _ (LTML.TypedTree (LSD.KindName kind) (LSD.TypeName type_) content)) =
            case content of
                (LTML.Tree heading children) -> do
                    emplacedChildren <- mapM emplaceTexts children
                    return $
                        Tree.Tree $
                            Tree.Node
                                (Tree.NodeHeader (Text.pack kind) (Text.pack type_) heading)
                                ( (\c -> Edge (fromMaybe "" (getTitle c)) c)
                                    <$> emplacedChildren
                                )
                (LTML.Leaf text) -> do
                    textElement <-
                        ExceptT $
                            createTextElement userID docID $
                                Text.pack kind
                    let textID = TextElement.identifier textElement
                    let textRev = TextElementRef docID textID
                    textRevision <-
                        ExceptT $
                            createTextRevision userID $
                                NewTextRevision
                                    { newTextRevisionParent = Nothing
                                    , newTextRevisionElement = textRev
                                    , newTextRevisionContent = text
                                    , newTextRevisionCommentAnchors = Vector.empty
                                    }
                    case textRevision of
                        TextRevision.NoConflict revision ->
                            return $ Tree.Leaf $ TextElementRevision textElement $ Just revision
                        _ ->
                            throwError $ Custom "Text Revision Conflict During Initial Document Creation."
    root <- emplaceTexts tree
    case root of
        Tree.Tree node -> do
            TreeRevision.TreeRevision header _ <-
                ExceptT $
                    createTreeRevision
                        userID
                        docID
                        (TextElement.identifier . TextRevision.textElement <$> node)
            return $ FullDocument doc $ Just $ TreeRevision.TreeRevision header node
        Tree.Leaf _ -> throwError $ Custom "Root is leaf :/"
  where
    -- Temporary function to get a somewhat usable title.
    -- Should be replaced by a function provided by the language team later on.
    getTitle :: Tree TextElementRevision -> Maybe Text
    getTitle x =
        typeTitle x
            <|> ((msum . (maybeTitle <$>) . Text.lines) =<< getContent x)
      where
        typeTitle :: Tree TextElementRevision -> Maybe Text
        typeTitle (Tree.Tree (Node header _)) =
            case (Tree.headerKind header, Tree.headerType header) of
                (_, "appendix") -> Just "Appendix"
                (_, "attachments") -> Just "Anlagen"
                ("document-mainbody", "inner") -> Just "Hauptteil"
                _ -> Nothing
        typeTitle _ = Nothing
        maybeTitle :: Text -> Maybe Text
        maybeTitle txt
            | "§" `Text.isPrefixOf` stripped = Just stripped
            | "!" `Text.isPrefixOf` stripped = Just stripped
            | "[intro]" `Text.isPrefixOf` stripped = Just "Intro"
            | "[extro]" `Text.isPrefixOf` stripped = Just "Extro"
            | otherwise = Nothing
          where
            stripped = Text.strip txt
        getContent :: Tree TextElementRevision -> Maybe Text
        getContent (Tree.Leaf (TextElementRevision _ rev)) = TextRevision.content <$> rev
        getContent (Tree.Tree (Tree.Node header _)) = Tree.heading header

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

guardSuperAdmin
    :: (HasIsSuperAdmin m)
    => UserID
    -> ExceptT Error m ()
guardSuperAdmin userID = do
    isSuperAdmin <- lift $ DB.isSuperAdmin userID
    unless isSuperAdmin $
        throwError SuperAdminOnly

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
            TreeRevision.Specific _ -> existsTreeRevision
            _ -> existsTreeRevision || allowLatestNothing
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
            TextRevision.Specific _ -> existsTextRevision
            _ -> existsTextRevision || allowLatestNothing
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
