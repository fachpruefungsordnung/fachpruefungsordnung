{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.DocsHandlers
    ( DocsAPI
    , docsServer
    , getUser
    , withDB
    ) where

import Data.Time (UTCTime)

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import Hasql.Connection (Connection)
import qualified Hasql.Session as Session

import Servant
    ( Capture
    , Delete
    , Description
    , Get
    , Handler
    , JSON
    , Post
    , QueryParam
    , ReqBody
    , Server
    , Summary
    , err400
    , err403
    , err500
    , errBody
    , throwError
    , type (:<|>) (..)
    , type (:>)
    )
import Servant.Auth.Server (Auth, AuthResult (Authenticated))

import Server.Auth (AuthMethod)
import qualified Server.Auth as Auth
import Server.HandlerUtil (errNotLoggedIn, tryGetDBConnection)

import UserManagement.User (UserID)

import qualified Docs
import Docs.Document (Document, DocumentID (..))
import Docs.DocumentHistory (DocumentHistory)
import Docs.Hasql.Database (run, runTransaction)
import Docs.TextElement
    ( TextElement
    , TextElementID
    , TextElementRef (..)
    , prettyPrintTextElementRef
    )
import Docs.TextRevision
    ( ConflictStatus
    , DraftRevision
    , NewTextRevision (..)
    , Rendered
    , TextElementRevision
    , TextRevisionHistory
    , TextRevisionRef (..)
    , TextRevisionSelector
    , prettyPrintTextRevisionRef
    )
import Docs.Tree (Node)
import Docs.TreeRevision
    ( TreeRevision
    , TreeRevisionHistory
    , TreeRevisionRef (..)
    , TreeRevisionSelector
    , prettyPrintTreeRevisionRef
    )

import Docs.Comment
    ( Comment
    , CommentID
    , CommentRef (CommentRef)
    , Message
    , prettyPrintCommentRef
    )
import Docs.FullDocument (FullDocument)
import Docs.MetaTree (TreeRevisionWithMetaData)
import Docs.Revision
    ( RevisionRef (RevisionRef)
    , RevisionSelector
    , prettyPrintRevisionRef
    )
import Language.Ltml.Tree.Example.Fpo (fpoTree)
import Server.DTOs.Comments (Comments (Comments))
import Server.DTOs.CreateComment (CreateComment)
import qualified Server.DTOs.CreateComment as CreateComment
import Server.DTOs.CreateDocument (CreateDocument)
import qualified Server.DTOs.CreateDocument as CreateDocument
import Server.DTOs.CreateReply (CreateReply)
import qualified Server.DTOs.CreateReply as CreateReply
import Server.DTOs.CreateTextElement (CreateTextElement)
import qualified Server.DTOs.CreateTextElement as CreateTextElement
import Server.DTOs.CreateTextRevision (CreateTextRevision)
import qualified Server.DTOs.CreateTextRevision as CreateTextRevision
import Server.DTOs.Documents
    ( Documents (Documents)
    , DocumentsQuery (DocumentsQuery)
    )
import qualified Server.DTOs.Documents as Documents
import Server.Handlers.RenderHandlers (RenderAPI, renderServer)
import UserManagement.Group (GroupID)

type DocsAPI =
    "docs"
        :> ( {-   -} PostDocument
                :<|> GetDocument
                :<|> GetDocuments
                :<|> PostTextElement
                :<|> PostTextRevision
                :<|> GetTextElementRevision
                :<|> PostTreeRevision
                :<|> GetTreeRevision
                :<|> GetTreeRevisionFull
                :<|> GetTextHistory
                :<|> GetTreeHistory
                :<|> GetDocumentHistory
                :<|> PostComment
                :<|> GetComments
                :<|> ResolveComment
                :<|> PostReply
                :<|> GetDocumentRevision
                :<|> GetDocumentRevisionTree
                :<|> GetDocumentRevisionText
                :<|> GetDraftTextRevision
                :<|> PublishDraftTextRevision
                :<|> DiscardDraftTextRevision
                :<|> RenderAPI
           )

type PostDocument =
    Summary "Create a new document"
        :> Description "Create a new document with default content"
        :> Auth AuthMethod Auth.Token
        :> ReqBody '[JSON] CreateDocument
        :> Post '[JSON] (FullDocument (Rendered TextElementRevision))

type GetDocument =
    Summary "Get metadata for a document"
        :> Description "Obtain a documents metadat"
        :> Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> Get '[JSON] Document

type GetDocuments =
    Summary "Get all documents"
        :> Description
            "Get all documents visible for the user. For super admins, this does not contain all visible documents, as a super admin can see all documents."
        :> Auth AuthMethod Auth.Token
        :> QueryParam "user" UserID
        :> QueryParam "group" GroupID
        :> Get '[JSON] Documents

type PostTextElement =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> ReqBody '[JSON] CreateTextElement
        :> Post '[JSON] TextElement

type PostTextRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "rev"
        :> QueryParam "isAutoSave" Bool
        :> ReqBody '[JSON] CreateTextRevision
        :> Post '[JSON] (Rendered ConflictStatus)

type GetTextElementRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "rev"
        :> Capture "textRevision" TextRevisionSelector
        :> Get '[JSON] (Maybe (Rendered TextElementRevision))

type PostTreeRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> ReqBody '[JSON] (Node TextElementID)
        :> Post '[JSON] (TreeRevisionWithMetaData TextElementID)

type GetTreeRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> Capture "treeRevision" TreeRevisionSelector
        :> Get '[JSON] (Maybe (TreeRevisionWithMetaData TextElement))

type GetTreeRevisionFull =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> Capture "treeRevision" TreeRevisionSelector
        :> "full"
        :> Get '[JSON] (Maybe (TreeRevisionWithMetaData TextElementRevision))

type GetTextHistory =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "history"
        :> QueryParam "after" UTCTime
        :> QueryParam "before" UTCTime
        :> QueryParam "limit" Docs.Limit
        :> Get '[JSON] TextRevisionHistory

type GetTreeHistory =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> "history"
        :> QueryParam "before" UTCTime
        :> QueryParam "limit" Docs.Limit
        :> Get '[JSON] TreeRevisionHistory

type GetDocumentHistory =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "history"
        :> QueryParam "before" UTCTime
        :> QueryParam "limit" Docs.Limit
        :> Get '[JSON] DocumentHistory

type PostComment =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "comments"
        :> ReqBody '[JSON] CreateComment
        :> Post '[JSON] Comment

type GetComments =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "comments"
        :> Get '[JSON] Comments

-- | jaja, das ist kein cleanes rest design,
--   aber das ist mir langsam auch wirklich scheiß egal.
type ResolveComment =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "comments"
        :> Capture "commentID" CommentID
        :> "resolve"
        :> Post '[JSON] ()

type PostReply =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "comments"
        :> Capture "commentID" CommentID
        :> "replies"
        :> ReqBody '[JSON] CreateReply
        :> Post '[JSON] Message

type GetDocumentRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "rev"
        :> Capture "revision" RevisionSelector
        :> Get '[JSON] (FullDocument TextElementRevision)

type GetDocumentRevisionTree =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "rev"
        :> Capture "revision" RevisionSelector
        :> "tree"
        :> Get '[JSON] (Maybe (TreeRevision TextElement))

type GetDocumentRevisionText =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "rev"
        :> Capture "revision" RevisionSelector
        :> "text"
        :> Capture "textElementID" TextElementID
        :> Get '[JSON] (Maybe TextElementRevision)

type GetDraftTextRevision =
    Summary "Get draft text revision"
        :> Description
            "Retrieve the user's draft text revision for a specific text element, if it exists"
        :> Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "draft"
        :> Get '[JSON] (Maybe DraftRevision)

type PublishDraftTextRevision =
    Summary "Publish draft text revision"
        :> Description
            "Publish the user's draft text revision to the main revision tree, potentially creating conflicts"
        :> Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "draft"
        :> "publish"
        :> Post '[JSON] (Rendered ConflictStatus)

type DiscardDraftTextRevision =
    Summary "Discard draft text revision"
        :> Description
            "Delete the user's draft text revision, discarding all unsaved changes"
        :> Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "draft"
        :> Delete '[JSON] ()

docsServer :: Server DocsAPI
docsServer =
    {-    -} postDocumentHandler
        :<|> getDocumentHandler
        :<|> getDocumentsHandler
        :<|> postTextElementHandler
        :<|> postTextRevisionHandler
        :<|> getTextElementRevisionHandler
        :<|> postTreeRevisionHandler
        :<|> getTreeRevisionHandler
        :<|> getTreeRevisionFullHandler
        :<|> getTextHistoryHandler
        :<|> getTreeHistoryHandler
        :<|> getDocumentHistoryHandler
        :<|> postCommentHandler
        :<|> getCommentsHandler
        :<|> resolveCommentHandler
        :<|> createReplyHandler
        :<|> getDocumentRevisionHandler
        :<|> getDocumentRevisionTreeHandler
        :<|> getDocumentRevisionTextHandler
        :<|> getDraftTextRevisionHandler
        :<|> publishDraftTextRevisionHandler
        :<|> discardDraftTextRevisionHandler
        :<|> renderServer

postDocumentHandler
    :: AuthResult Auth.Token
    -> CreateDocument
    -> Handler (FullDocument (Rendered TextElementRevision))
postDocumentHandler auth doc = do
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.newDefaultDocument
                userID
                (CreateDocument.groupID doc)
                (CreateDocument.title doc)
                fpoTree

getDocumentHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> Handler Document
getDocumentHandler auth docID = do
    userID <- getUser auth
    withDB $ run $ Docs.getDocument userID docID

getDocumentsHandler
    :: AuthResult Auth.Token
    -> Maybe UserID
    -> Maybe GroupID
    -> Handler Documents
getDocumentsHandler auth byUserID byGroupID = do
    userID <- getUser auth
    result <- withDB $ run $ Docs.getDocuments userID byUserID byGroupID
    return $
        Documents
            { Documents.documents = result
            , Documents.query =
                DocumentsQuery
                    { Documents.user = byUserID
                    , Documents.group = byGroupID
                    }
            }

postTextElementHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> CreateTextElement
    -> Handler TextElement
postTextElementHandler auth docID element = do
    userID <- getUser auth
    withDB $
        run $
            Docs.createTextElement
                userID
                docID
                (CreateTextElement.kind element)
                (CreateTextElement.type_ element)

postTextRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> Maybe Bool
    -> CreateTextRevision
    -> Handler (Rendered ConflictStatus)
postTextRevisionHandler auth docID textID mIsAutoSave revision = do
    let isAutoSave = fromMaybe False mIsAutoSave -- Default to False if not provided
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.createTextRevision userID $
                NewTextRevision
                    (TextElementRef docID textID)
                    (CreateTextRevision.parent revision)
                    (CreateTextRevision.content revision)
                    (CreateTextRevision.commentAnchors revision)
                    isAutoSave

getTextElementRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> TextRevisionSelector
    -> Handler (Maybe (Rendered TextElementRevision))
getTextElementRevisionHandler auth docID textID revision = do
    userID <- getUser auth
    withDB $
        run $
            Docs.getTextElementRevision userID $
                TextRevisionRef (TextElementRef docID textID) revision

postTreeRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> Node TextElementID
    -> Handler (TreeRevisionWithMetaData TextElementID)
postTreeRevisionHandler auth docID node = do
    userID <- getUser auth
    withDB $ runTransaction $ Docs.createTreeRevision userID docID node

getTreeRevisionFullHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TreeRevisionSelector
    -> Handler (Maybe (TreeRevisionWithMetaData TextElementRevision))
getTreeRevisionFullHandler auth docID revision = do
    userID <- getUser auth
    withDB $
        run $
            Docs.getFullTreeRevision userID $
                TreeRevisionRef docID revision

getTreeRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TreeRevisionSelector
    -> Handler (Maybe (TreeRevisionWithMetaData TextElement))
getTreeRevisionHandler auth docID revision = do
    userID <- getUser auth
    withDB $ run $ Docs.getTreeRevision userID $ TreeRevisionRef docID revision

getTextHistoryHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> Maybe UTCTime
    -> Maybe UTCTime
    -> Maybe Docs.Limit
    -> Handler TextRevisionHistory
getTextHistoryHandler auth docID textID after before limit = do
    userID <- getUser auth
    withDB $
        run $
            Docs.getTextHistory
                userID
                (TextElementRef docID textID)
                after
                before
                limit

getTreeHistoryHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> Maybe UTCTime
    -> Maybe Docs.Limit
    -> Handler TreeRevisionHistory
getTreeHistoryHandler auth docID before limit = do
    userID <- getUser auth
    withDB $ run $ Docs.getTreeHistory userID docID before limit

getDocumentHistoryHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> Maybe UTCTime
    -> Maybe Docs.Limit
    -> Handler DocumentHistory
getDocumentHistoryHandler auth docID before limit = do
    userID <- getUser auth
    withDB $ run $ Docs.getDocumentHistory userID docID before limit

postCommentHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> CreateComment
    -> Handler Comment
postCommentHandler auth docID textID comment = do
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.createComment
                userID
                (TextElementRef docID textID)
                (CreateComment.text comment)

getCommentsHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> Handler Comments
getCommentsHandler auth docID textID = do
    userID <- getUser auth
    comments <-
        withDB $
            run $
                Docs.getComments
                    userID
                    (TextElementRef docID textID)
    return $ Comments comments

resolveCommentHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> CommentID
    -> Handler ()
resolveCommentHandler auth docID textID commentID = do
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.resolveComment userID (CommentRef (TextElementRef docID textID) commentID)

createReplyHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> CommentID
    -> CreateReply
    -> Handler Message
createReplyHandler auth docID textID commentID bodyDTO = do
    userID <- getUser auth
    withDB
        $ runTransaction
        $ Docs.createReply
            userID
            (CommentRef (TextElementRef docID textID) commentID)
        $ CreateReply.text
            bodyDTO

getDocumentRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> RevisionSelector
    -> Handler (FullDocument TextElementRevision)
getDocumentRevisionHandler auth docID rev = do
    userID <- getUser auth
    withDB $ run $ Docs.getDocumentRevision userID (RevisionRef docID rev)

getDocumentRevisionTreeHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> RevisionSelector
    -> Handler (Maybe (TreeRevision TextElement))
getDocumentRevisionTreeHandler auth docID rev = do
    userID <- getUser auth
    withDB $ run $ Docs.getDocumentRevisionTree userID (RevisionRef docID rev)

getDocumentRevisionTextHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> RevisionSelector
    -> TextElementID
    -> Handler (Maybe TextElementRevision)
getDocumentRevisionTextHandler auth docID rev textID = do
    userID <- getUser auth
    withDB $
        run $
            Docs.getDocumentRevisionText
                userID
                (RevisionRef docID rev)
                textID

-- utililty

getUser :: AuthResult Auth.Token -> Handler UserID
getUser (Authenticated Auth.Token {..}) = return subject
getUser _ = throwError errNotLoggedIn

withDB
    :: (Connection -> IO (Either Session.SessionError (Docs.Result a)))
    -> Handler a
withDB io = do
    db <- tryGetDBConnection
    result <- liftIO $ io db
    guardedDBAccess result

guardedDBAccess
    :: Either Session.SessionError (Docs.Result a)
    -> Handler a
guardedDBAccess result = guardDBResult result >>= guardDocsResult

guardDBResult :: Either Session.SessionError a -> Handler a
guardDBResult (Right ok) = return ok
guardDBResult (Left err) =
    throwError $
        err500
            { errBody = LBS.pack $ "Database error: " ++ show err ++ "\n"
            }

guardDocsResult :: Docs.Result a -> Handler a
guardDocsResult (Right ok) = return ok
guardDocsResult (Left err) = throwError $ mapErr err
  where
    mapErr (Docs.NoPermission docID perms) =
        err403
            { errBody =
                LBS.pack $
                    "You are not allowed to "
                        ++ show perms
                        ++ " document "
                        ++ show (unDocumentID docID)
                        ++ "!\n"
            }
    mapErr (Docs.NoPermissionForUser userID) =
        err403
            { errBody =
                LBS.pack $
                    "You are not allowed to view information about "
                        ++ show userID
                        ++ "!\n"
            }
    mapErr (Docs.NoPermissionInGroup groupID) =
        err403
            { errBody =
                LBS.pack $
                    "You are not an admin in group "
                        ++ show groupID
                        ++ "!\n"
            }
    mapErr Docs.SuperAdminOnly =
        err403
            { errBody =
                LBS.pack
                    "This feature is only for super admins!\n"
            }
    mapErr (Docs.DocumentNotFound docID) =
        err400
            { errBody =
                LBS.pack $
                    "Document "
                        ++ show (unDocumentID docID)
                        ++ " not found!\n"
            }
    mapErr (Docs.TextElementNotFound ref) =
        err400
            { errBody =
                LBS.pack $
                    "TextElement "
                        ++ prettyPrintTextElementRef ref
                        ++ " not found!\n"
            }
    mapErr (Docs.RevisionNotFound ref) =
        err400
            { errBody =
                LBS.pack $
                    "TextRevision "
                        ++ prettyPrintRevisionRef ref
                        ++ " not found!\n"
            }
    mapErr (Docs.TextRevisionNotFound ref) =
        err400
            { errBody =
                LBS.pack $
                    "TextRevision "
                        ++ prettyPrintTextRevisionRef ref
                        ++ " not found!\n"
            }
    mapErr (Docs.TreeRevisionNotFound ref) =
        err400
            { errBody =
                LBS.pack $
                    "TreeRevision "
                        ++ prettyPrintTreeRevisionRef ref
                        ++ " not found!\n"
            }
    mapErr (Docs.CommentNotFound ref) =
        err400
            { errBody =
                LBS.pack $
                    "Comment "
                        ++ prettyPrintCommentRef ref
                        ++ " not found!\n"
            }
    mapErr (Docs.Custom msg) =
        err400
            { errBody =
                LBS.pack $
                    T.unpack msg ++ "\n"
            }

-- | Get draft text revision for current user
getDraftTextRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> Handler (Maybe DraftRevision)
getDraftTextRevisionHandler auth docID textID = do
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.getDraftTextRevision userID $
                TextElementRef docID textID

-- | Publish draft text revision to main revision tree
publishDraftTextRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> Handler (Rendered ConflictStatus)
publishDraftTextRevisionHandler auth docID textID = do
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.publishDraftTextRevision userID $
                TextElementRef docID textID

-- | Discard draft text revision
discardDraftTextRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> Handler ()
discardDraftTextRevisionHandler auth docID textID = do
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.discardDraftTextRevision userID $
                TextElementRef docID textID
