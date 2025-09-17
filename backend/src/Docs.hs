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
    , getTextRevisionPDF
    , getDocumentRevisionText
    , getTreeRevision
    , getDocumentRevisionTree
    , createTreeRevision
    , getFullTreeRevision
    , getTextHistory
    , getTreeHistory
    , getDocumentHistory
    , getDocumentRevision
    , getTreeRevisionPDF
    , getTreeRevisionHTML
    , getDocumentRevisionPDF
    , getDocumentRevisionHTML
    , createComment
    , getComments
    , resolveComment
    , createReply
    , getLogs
    , getDraftTextRevision
    , publishDraftTextRevision
    , discardDraftTextRevision
    ) where

import Control.Monad (join, unless)
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

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (bimap, first, second))
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.OpenApi (ToSchema)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
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
import Docs.LTML
    ( nodeToLtmlInputTreePred
    , treeRevisionToMeta
    )
import Docs.MetaTree (TreeRevisionWithMetaData (TreeRevisionWithMetaData))
import Docs.Renderable (directRenderable)
import qualified Docs.Renderable as Renderable
import Docs.Rendered
    ( HTMLBytes (HTMLBytes, unHTMLBytes)
    , PDFBytes (PDFBytes)
    , ZipBytes (ZipBytes)
    )
import Docs.Revision
    ( RevisionRef (RevisionRef)
    , textRevisionRefFor
    , treeRevisionRefFor
    )
import qualified Docs.Revision as Revision
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementKind
    , TextElementRef (..)
    , TextElementType
    )
import qualified Docs.TextElement as TextElement
import Docs.TextRevision
    ( ConflictStatus
    , DraftRevision
    , NewTextRevision (..)
    , Rendered (Rendered)
    , TextElementRevision (TextElementRevision)
    , TextRevisionHistory
    , TextRevisionRef (..)
    )
import qualified Docs.TextRevision as TextRevision
import Docs.Tree (Node)
import qualified Docs.Tree as Tree
import Docs.TreeRevision
    ( TreeRevision (TreeRevision)
    , TreeRevisionHistory
    , TreeRevisionRef (..)
    )
import qualified Docs.TreeRevision as TreeRevision
import qualified Docs.UserRef as UserRef
import GHC.Generics (Generic)
import GHC.Int (Int64)
import qualified Language.Ltml.AST.DocumentContainer as LTML
import qualified Language.Ltml.HTML as HTML
import qualified Language.Ltml.HTML.Export as HTML
import qualified Language.Ltml.ToLaTeX.PDFGenerator as PDF
import qualified Language.Ltml.Tree.ToLtml as LTML
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
    | PDFError Text
    | ZipHTMLError
    | Custom Text
    deriving (Generic)

instance ToJSON Error

instance FromJSON Error

instance ToSchema Error

type Result a = Either Error a

type Limit = Int64

defaultHistoryLimit :: Limit
defaultHistoryLimit = 200

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
    -> TextElementType
    -> m (Result TextElement)
createTextElement userID docID kind type_ =
    logged userID Scope.docsText $
        runExceptT $ do
            guardPermission Edit docID userID
            guardExistsDocument docID
            lift $ DB.createTextElement docID kind type_

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
       , HasGetTreeRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , DB.HasDraftTextRevision m
       )
    => UserID
    -> NewTextRevision
    -> m (Result (Rendered ConflictStatus))
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
                lift $
                    DB.createTextRevision
                        userID
                        ref
                        (newTextRevisionContent revision)
                        (newTextRevisionCommentAnchors revision)
        do
            now <- lift DB.now
            let render =
                    rendered'
                        userID
                        (TextRevisionRef ref TextRevision.Latest)
                        (newTextRevisionContent revision)
            case latestRevision of
                -- first revision
                Nothing -> do
                    createdRevision <- createRevision
                    let result = TextRevision.NoConflict createdRevision
                    render result
                -- render . TextRevision.NoConflict <$> createRevision
                Just latest
                    -- content has not changed? -> return latest
                    | TextRevision.contentsNotChanged latest revision ->
                        render $ TextRevision.NoConflict latest
                    -- no conflict, and can update? -> update (squash)
                    | latestRevisionID == parentRevisionID && shouldUpdate now latest -> do
                        newRevision <-
                            lift $
                                DB.updateTextRevision
                                    (identifier latest)
                                    (newTextRevisionContent revision)
                                    (newTextRevisionCommentAnchors revision)
                        render $ TextRevision.NoConflict newRevision
                    -- no conflict, but can not update? -> create new
                    | latestRevisionID == parentRevisionID -> do
                        createdRevision <- createRevision
                        render $ TextRevision.NoConflict createdRevision
                    -- conflict
                    | otherwise ->
                        if newTextRevisionIsAutoSave revision
                            then do
                                -- For autosave conflicts, create a draft revision
                                draftRevision <-
                                    lift $
                                        DB.createDraftTextRevision
                                            userID
                                            ref
                                            (identifier latest)
                                            (newTextRevisionContent revision)
                                            (newTextRevisionCommentAnchors revision)
                                render $
                                    TextRevision.DraftCreated
                                        draftRevision
                                        (identifier latest)
                            else -- For manual save conflicts, return conflict
                                render $
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
    :: ( HasGetTextElementRevision m
       , HasGetTreeRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , HasLogMessage m
       )
    => UserID
    -> TextRevisionRef
    -> m (Result (Maybe (Rendered TextElementRevision)))
getTextElementRevision userID ref = logged userID Scope.docsTextRevision $
    runExceptT $ do
        let (TextRevisionRef (TextElementRef docID _) _) = ref
        guardPermission Read docID userID
        guardExistsTextRevision True ref
        revision <- lift $ DB.getTextElementRevision ref
        mapM (renderTextElementRevision userID ref) revision

renderTextElementRevision
    :: ( HasGetTreeRevision m
       , HasLogMessage m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       )
    => UserID
    -> TextRevisionRef
    -> TextElementRevision
    -> ExceptT Error m (Rendered TextElementRevision)
renderTextElementRevision userID ref rev = rendered' userID ref content rev
  where
    content = maybe "" TextRevision.content $ TextRevision.revision rev

rendered'
    :: ( HasGetTreeRevision m
       , HasLogMessage m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       )
    => UserID
    -> TextRevisionRef
    -> Text
    -> a
    -> ExceptT Error m (Rendered a)
rendered' userID ref content element = do
    html <- ExceptT $ getTextRevisionHTMLForCustomText userID ref content
    let html' =
            -- TODO: unHTMLBytes hier n bissl dumm, warum nicht den richtigen typen nuitzen????
            either (const Nothing) Just . TE.decodeUtf8' . BL.toStrict . unHTMLBytes $ html
    return $
        Rendered
            { TextRevision.element = element
            , TextRevision.html = fromMaybe "" html'
            }

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
    :: ( HasCreateTreeRevision m
       , HasLogMessage m
       , HasGetTextElementRevision m
       , HasGetTreeRevision m
       )
    => UserID
    -> DocumentID
    -> Node TextElementID
    -> m (Result (TreeRevisionWithMetaData TextElementID))
createTreeRevision userID docID root = logged userID Scope.docsTreeRevision $
    runExceptT $ do
        guardPermission Edit docID userID
        guardExistsDocument docID
        existsTextElement <- lift $ DB.existsTextElementInDocument docID
        (TreeRevision header _) <- case firstFalse existsTextElement root of
            Just textID -> throwError $ TextElementNotFound $ TextElementRef docID textID
            Nothing -> lift $ DB.createTreeRevision userID docID root
        newTree <-
            getTreeRevision' userID
                $ TreeRevisionRef
                    docID
                $ TreeRevision.Specific
                    (TreeRevision.identifier header)
        newTree' <-
            ExceptT . pure $
                maybe
                    (Left (Custom "The revision I just created is gone :((("))
                    Right
                    newTree
        return $ TextElement.identifier <$> newTree'
  where
    firstFalse predicate = find (not . predicate)

getFullTreeRevision
    :: (HasGetTreeRevision m, HasLogMessage m, HasGetTextElementRevision m)
    => UserID
    -> TreeRevisionRef
    -> m (Result (Maybe (TreeRevisionWithMetaData TextElementRevision)))
getFullTreeRevision userID =
    logged userID Scope.docsTreeRevision
        . runExceptT
        . getFullTreeRevision' userID

getFullTreeRevision'
    :: (HasGetTreeRevision m, HasLogMessage m, HasGetTextElementRevision m)
    => UserID
    -> TreeRevisionRef
    -> ExceptT Error m (Maybe (TreeRevisionWithMetaData TextElementRevision))
getFullTreeRevision' userID ref = do
    fullTree <- getTreeWithLatestTexts userID ref
    ExceptT . pure $ case fullTree of
        Just tree -> bimap (Custom . Text.pack . show) Just (treeRevisionToMeta tree)
        Nothing -> Right Nothing

getTextRevisionPDF
    :: ( HasGetTreeRevision m
       , HasLogMessage m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , MonadIO m
       )
    => UserID
    -> TextRevisionRef
    -> m (Result PDFBytes)
getTextRevisionPDF userID ref@(TextRevisionRef (TextElementRef _ textID) _) =
    logged userID Scope.docsTreeRevision $ runExceptT $ do
        guardExistsTextRevision True ref
        let ref' = Revision.refFromTextRevision ref
        maybeDocumentContainer <-
            getDocumentRevisionDocumentContainerForTextElement userID ref' textID
        documentContainer <-
            ExceptT . pure $
                maybe (Left $ RevisionNotFound ref') Right maybeDocumentContainer
        toPDF documentContainer

getTextRevisionHTMLForCustomText
    :: ( HasGetTreeRevision m
       , HasLogMessage m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       )
    => UserID
    -> TextRevisionRef
    -> Text
    -> m (Result HTMLBytes)
getTextRevisionHTMLForCustomText userID ref@(TextRevisionRef (TextElementRef _ textID) _) text =
    logged userID Scope.docsTreeRevision $ runExceptT $ do
        guardExistsTextRevision True ref
        let ref' = Revision.refFromTextRevision ref
        maybeDocumentContainer <-
            getDocumentRevisionDocumentContainerForCustomText userID ref' textID text
        return $
            HTMLBytes $
                maybe "" HTML.renderHtmlCssBS maybeDocumentContainer

getTreeRevisionPDF
    :: (HasGetTreeRevision m, HasLogMessage m, HasGetTextElementRevision m, MonadIO m)
    => UserID
    -> TreeRevisionRef
    -> m (Result PDFBytes)
getTreeRevisionPDF userID ref = logged userID Scope.docsTreeRevision $ runExceptT $ do
    maybeDocumentContainer <- getTreeRevisionDocumentContainer userID ref
    documentContainer <-
        ExceptT . pure $
            maybe (Left $ TreeRevisionNotFound ref) Right maybeDocumentContainer
    toPDF documentContainer

getTreeRevisionHTML
    :: (HasGetTreeRevision m, HasLogMessage m, HasGetTextElementRevision m, MonadIO m)
    => UserID
    -> TreeRevisionRef
    -> m (Result ZipBytes)
getTreeRevisionHTML userID ref = logged userID Scope.docsTreeRevision $ runExceptT $ do
    maybeDocumentContainer <- getTreeRevisionDocumentContainer userID ref
    documentContainer <-
        ExceptT . pure $
            maybe (Left $ TreeRevisionNotFound ref) Right maybeDocumentContainer
    toHTML documentContainer

toPDF
    :: (MonadIO m)
    => LTML.Flagged' LTML.DocumentContainer
    -> ExceptT Error m PDFBytes
toPDF documentContainer = do
    pdf <- liftIO $ PDF.generatePDF documentContainer
    ExceptT . pure $ bimap (PDFError . Text.pack) PDFBytes pdf

toHTML
    :: (MonadIO m)
    => LTML.Flagged' LTML.DocumentContainer
    -> ExceptT Error m ZipBytes
toHTML documentContainer = do
    zipBytes <- liftIO $ HTML.renderZip documentContainer
    ExceptT . pure $ maybe (Left ZipHTMLError) (Right . ZipBytes) zipBytes

getTreeRevisionDocumentContainer
    :: (HasGetTreeRevision m, HasLogMessage m, HasGetTextElementRevision m)
    => UserID
    -> TreeRevisionRef
    -> ExceptT Error m (Maybe (LTML.Flagged' LTML.DocumentContainer))
getTreeRevisionDocumentContainer userID ref = do
    fullRevision <- getTreeWithLatestTexts userID ref
    ExceptT . pure $
        maybe
            (Right Nothing)
            (second Just . toDocumentContainer)
            fullRevision

toDocumentContainer
    :: TreeRevision TextElementRevision
    -> Result (LTML.Flagged' LTML.DocumentContainer)
toDocumentContainer fullRevision =
    first (Custom . Text.pack . show) $
        LTML.treeToLtml $
            toInputTree fullRevision
  where
    toInputTree (TreeRevision _ node) =
        nodeToLtmlInputTreePred (const True) (const True) node

toDocumentContainerForCustomText
    :: TextElementID
    -> Text
    -> TreeRevision TextElementRevision
    -> Result (LTML.Flagged' LTML.DocumentContainer)
toDocumentContainerForCustomText textID text fullRevision =
    let overridenRevision = override <$> fullRevision
     in first (Custom . Text.pack . show) $
            LTML.treeToLtml $
                toInputTree overridenRevision
  where
    toInputTree (TreeRevision _ node) =
        nodeToLtmlInputTreePred
            (const False)
            ((textID ==) . Renderable.identifier)
            node
    override rev@(TextElementRevision textElement _) =
        let id_ = TextElement.identifier textElement
         in if id_ == textID
                then
                    (directRenderable rev id_)
                        { Renderable.content = text
                        }
                else directRenderable rev id_

toDocumentContainerForTextElement
    :: TextElementID
    -> TreeRevision TextElementRevision
    -> Result (LTML.Flagged' LTML.DocumentContainer)
toDocumentContainerForTextElement teID fullRevision =
    first (Custom . Text.pack . show) $
        LTML.treeToLtml $
            toInputTree fullRevision
  where
    toInputTree (TreeRevision _ node) =
        nodeToLtmlInputTreePred
            (const False)
            ((teID ==) . TextElement.identifier . TextRevision.textElement)
            node

getDocumentRevisionDocumentContainerForTextElement
    :: ( HasGetTreeRevision m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , HasLogMessage m
       )
    => UserID
    -> RevisionRef
    -> TextElementID
    -> ExceptT Error m (Maybe (LTML.Flagged' LTML.DocumentContainer))
getDocumentRevisionDocumentContainerForTextElement userID ref textID = do
    fullRevision <- getDocumentRevision' userID ref <&> FullDocument.body
    ExceptT . pure $
        maybe
            (Right Nothing)
            (second Just . toDocumentContainerForTextElement textID)
            fullRevision

getDocumentRevisionDocumentContainerForCustomText
    :: ( HasGetTreeRevision m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , HasLogMessage m
       )
    => UserID
    -> RevisionRef
    -> TextElementID
    -> Text
    -> ExceptT Error m (Maybe (LTML.Flagged' LTML.DocumentContainer))
getDocumentRevisionDocumentContainerForCustomText userID ref textID text = do
    fullRevision <- getDocumentRevision' userID ref <&> FullDocument.body
    ExceptT . pure $
        maybe
            (Right Nothing)
            (second Just . toDocumentContainerForCustomText textID text)
            fullRevision

getTreeRevision
    :: (HasGetTreeRevision m, HasLogMessage m, HasGetTextElementRevision m)
    => UserID
    -> TreeRevisionRef
    -> m (Result (Maybe (TreeRevisionWithMetaData TextElement)))
getTreeRevision userID =
    logged userID Scope.docsTreeRevision
        . runExceptT
        . getTreeRevision' userID

getTreeRevision'
    :: (HasGetTreeRevision m, HasLogMessage m, HasGetTextElementRevision m)
    => UserID
    -> TreeRevisionRef
    -> ExceptT Error m (Maybe (TreeRevisionWithMetaData TextElement))
getTreeRevision' userID ref =
    -- ich möchte nicht drüber reden.
    ((TextRevision.textElement <$>) <$>) <$> getFullTreeRevision' userID ref

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
    -> Maybe UTCTime
    -> Maybe Limit
    -> m (Result TextRevisionHistory)
getTextHistory userID ref@(TextElementRef docID _) from to limit = logged userID Scope.docsText $
    runExceptT $ do
        guardPermission Read docID userID
        guardExistsTextElement ref
        lift $ DB.getTextHistory ref from to $ fromMaybe defaultHistoryLimit limit

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
    -> ExceptT Error m (Maybe (TreeRevision TextElementRevision))
getTreeWithLatestTexts userID revision = do
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

getDocumentRevisionPDF
    :: ( HasGetTreeRevision m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , HasLogMessage m
       , MonadIO m
       )
    => UserID
    -> RevisionRef
    -> m (Result PDFBytes)
getDocumentRevisionPDF userID ref =
    logged userID Scope.docsTreeRevision $ runExceptT $ do
        maybeDocumentContainer <- getDocumentRevisionDocumentContainer userID ref
        documentContainer <-
            ExceptT . pure $
                maybe
                    (Left $ RevisionNotFound ref)
                    Right
                    maybeDocumentContainer
        toPDF documentContainer

getDocumentRevisionHTML
    :: ( HasGetTreeRevision m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , HasLogMessage m
       , MonadIO m
       )
    => UserID
    -> RevisionRef
    -> m (Result ZipBytes)
getDocumentRevisionHTML userID ref =
    logged userID Scope.docsTreeRevision $ runExceptT $ do
        maybeDocumentContainer <- getDocumentRevisionDocumentContainer userID ref
        documentContainer <-
            ExceptT . pure $
                maybe
                    (Left $ RevisionNotFound ref)
                    Right
                    maybeDocumentContainer
        toHTML documentContainer

getDocumentRevisionDocumentContainer
    :: ( HasGetTreeRevision m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , HasLogMessage m
       )
    => UserID
    -> RevisionRef
    -> ExceptT Error m (Maybe (LTML.Flagged' LTML.DocumentContainer))
getDocumentRevisionDocumentContainer userID ref = do
    fullRevision <- getDocumentRevision' userID ref <&> FullDocument.body
    ExceptT . pure $
        maybe
            (Right Nothing)
            (second Just . toDocumentContainer)
            fullRevision

getDocumentRevision
    :: ( HasGetTreeRevision m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , HasLogMessage m
       )
    => UserID
    -> RevisionRef
    -> m (Result (FullDocument TextElementRevision))
getDocumentRevision userID =
    logged userID Scope.docs . runExceptT . getDocumentRevision' userID

getDocumentRevision'
    :: ( HasGetTreeRevision m
       , HasGetTextElementRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , HasLogMessage m
       )
    => UserID
    -> RevisionRef
    -> ExceptT Error m (FullDocument TextElementRevision)
getDocumentRevision' userID ref@(RevisionRef docID _) = do
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
       , HasGetTreeRevision m
       , HasGetRevisionKey m
       , HasGetDocument m
       , DB.HasDraftTextRevision m
       )
    => UserID
    -> GroupID
    -> Text
    -> LTML.FlaggedInputTree'
    -> m (Result (FullDocument (Rendered TextElementRevision)))
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
                                emplacedChildren
                (LTML.Leaf text) -> do
                    textElement <-
                        ExceptT $
                            createTextElement
                                userID
                                docID
                                (Text.pack kind)
                                (Text.pack type_)
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
                                    , newTextRevisionIsAutoSave = False -- Document creation is not autosave
                                    }
                    case TextRevision.element textRevision of
                        TextRevision.NoConflict revision ->
                            return $
                                Tree.Leaf $
                                    Rendered
                                        (TextElementRevision textElement $ Just revision)
                                        (TextRevision.html textRevision)
                        _ ->
                            throwError $ Custom "Text Revision Conflict During Initial Document Creation."
    root <- emplaceTexts tree
    case root of
        Tree.Tree node -> do
            TreeRevisionWithMetaData header _ <-
                ExceptT $
                    createTreeRevision
                        userID
                        docID
                        ( (TextElement.identifier . TextRevision.textElement) . TextRevision.element
                            <$> node
                        )
            return $ FullDocument doc $ Just $ TreeRevision.TreeRevision header node
        Tree.Leaf _ -> throwError $ Custom "Root is leaf :/"

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

-- | Get draft text revision for a user and text element.
-- Returns Nothing if no draft exists for this user/element combination.
-- Drafts are user-specific and element-specific (one draft per user per text element).
getDraftTextRevision
    :: (DB.HasDraftTextRevision m, HasLogMessage m)
    => UserID
    -> TextElementRef
    -> m (Result (Maybe DraftRevision))
getDraftTextRevision userID ref@(TextElementRef docID _) = logged userID Scope.docsTextRevision $ runExceptT $ do
    guardPermission Read docID userID
    guardExistsTextElement ref
    lift $ DB.getDraftTextRevision userID ref

-- | Publish a draft text revision to the main revision tree.
-- This attempts to create a regular text revision from the draft content.
-- If conflicts occur, they are handled as errors (since publishing is explicit, not auto-save).
-- On successful publish, the draft is automatically discarded.
publishDraftTextRevision
    :: ( DB.HasDraftTextRevision m
       , HasCreateTextRevision m
       , HasGetTextElementRevision m
       , HasExistsComment m
       , HasGetRevisionKey m
       , HasGetDocument m
       , HasGetTreeRevision m
       , HasLogMessage m
       )
    => UserID
    -> TextElementRef
    -> m (Result (Rendered ConflictStatus))
publishDraftTextRevision userID ref@(TextElementRef docID _) = logged userID Scope.docsTextRevision $ runExceptT $ do
    guardPermission Edit docID userID
    guardExistsTextElement ref

    -- Get the current draft
    maybeDraft <- lift $ DB.getDraftTextRevision userID ref
    case maybeDraft of
        Nothing -> throwError $ Custom "No draft found for this text element"
        Just draft -> do
            -- Create a new regular revision from the draft
            let draftHeader = TextRevision.draftHeader draft
            let basedOnRevisionID = TextRevision.basedOnRevision draftHeader
            let newRevision =
                    NewTextRevision
                        { newTextRevisionElement = ref
                        , newTextRevisionParent = Just basedOnRevisionID
                        , newTextRevisionContent = TextRevision.draftContent draft
                        , newTextRevisionCommentAnchors = TextRevision.draftCommentAnchors draft
                        , newTextRevisionIsAutoSave = False -- Publishing is explicit, not auto
                        }

            -- Create the revision (may conflict with newer changes)
            result <- ExceptT $ createTextRevision userID newRevision

            -- If successful, delete the draft
            case TextRevision.element result of
                TextRevision.NoConflict _ -> do
                    lift $ DB.deleteDraftTextRevision userID ref
                    return result
                _ -> return result -- Keep draft on conflict

-- | Discard a draft text revision, permanently deleting all unsaved changes.
-- This operation cannot be undone. The draft is completely removed from storage.
discardDraftTextRevision
    :: (DB.HasDraftTextRevision m, HasLogMessage m)
    => UserID
    -> TextElementRef
    -> m (Result ())
discardDraftTextRevision userID ref@(TextElementRef docID _) = logged userID Scope.docsTextRevision $ runExceptT $ do
    guardPermission Edit docID userID
    guardExistsTextElement ref
    lift $ DB.deleteDraftTextRevision userID ref
