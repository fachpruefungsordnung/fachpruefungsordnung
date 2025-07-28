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
    ) where

import Data.Time (UTCTime)

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.ByteString.Lazy.Char8 as LBS

import Hasql.Connection (Connection)
import qualified Hasql.Session as Session

import Servant
    ( Capture
    , Get
    , Handler
    , JSON
    , Post
    , QueryParam
    , ReqBody
    , Server
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
    , NewTextRevision (..)
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

import Server.DTOs.CreateDocument (CreateDocument)
import qualified Server.DTOs.CreateDocument as CreateDocument
import Server.DTOs.CreateTextElement (CreateTextElement)
import qualified Server.DTOs.CreateTextElement as CreateTextElement
import Server.DTOs.CreateTextRevision (CreateTextRevision)
import qualified Server.DTOs.CreateTextRevision as CreateTextRevision
import Server.DTOs.Documents (Documents (Documents))
import qualified Server.DTOs.Documents as Documents

type DocsAPI =
    "v2"
        :> "docs"
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
           )

type PostDocument =
    Auth AuthMethod Auth.Token
        :> ReqBody '[JSON] CreateDocument
        :> Post '[JSON] Document

type GetDocument =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> Get '[JSON] Document

type GetDocuments =
    Auth AuthMethod Auth.Token
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
        :> ReqBody '[JSON] CreateTextRevision
        :> Post '[JSON] ConflictStatus

type GetTextElementRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "rev"
        :> Capture "textRevision" TextRevisionSelector
        :> Get '[JSON] (Maybe TextElementRevision)

type PostTreeRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> ReqBody '[JSON] (Node TextElementID)
        :> Post '[JSON] (TreeRevision TextElementID)

type GetTreeRevision =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> Capture "treeRevision" TreeRevisionSelector
        :> Get '[JSON] (TreeRevision TextElement)

type GetTreeRevisionFull =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "tree"
        :> Capture "treeRevision" TreeRevisionSelector
        :> "full"
        :> Get '[JSON] (TreeRevision TextElementRevision)

type GetTextHistory =
    Auth AuthMethod Auth.Token
        :> Capture "documentID" DocumentID
        :> "text"
        :> Capture "textElementID" TextElementID
        :> "history"
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

postDocumentHandler
    :: AuthResult Auth.Token
    -> CreateDocument
    -> Handler Document
postDocumentHandler auth doc = do
    userID <- getUser auth
    withDB $
        run $
            Docs.createDocument
                userID
                (CreateDocument.groupID doc)
                (CreateDocument.title doc)

getDocumentHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> Handler Document
getDocumentHandler auth docID = do
    userID <- getUser auth
    withDB $ run $ Docs.getDocument userID docID

getDocumentsHandler
    :: AuthResult Auth.Token
    -> Handler Documents
getDocumentsHandler auth = do
    userID <- getUser auth
    result <- withDB $ run $ Docs.getDocuments userID
    return $
        Documents
            { Documents.documents = result
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

postTextRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> CreateTextRevision
    -> Handler ConflictStatus
postTextRevisionHandler auth docID textID revision = do
    userID <- getUser auth
    withDB $
        runTransaction $
            Docs.createTextRevision userID $
                NewTextRevision
                    (TextElementRef docID textID)
                    (CreateTextRevision.parent revision)
                    (CreateTextRevision.content revision)

getTextElementRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> TextRevisionSelector
    -> Handler (Maybe TextElementRevision)
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
    -> Handler (TreeRevision TextElementID)
postTreeRevisionHandler auth docID node = do
    userID <- getUser auth
    withDB $ runTransaction $ Docs.createTreeRevision userID docID node

getTreeRevisionFullHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TreeRevisionSelector
    -> Handler (TreeRevision TextElementRevision)
getTreeRevisionFullHandler auth docID revision = do
    userID <- getUser auth
    withDB $
        run $
            Docs.getTreeWithLatestTexts userID $
                TreeRevisionRef docID revision

getTreeRevisionHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TreeRevisionSelector
    -> Handler (TreeRevision TextElement)
getTreeRevisionHandler auth docID revision = do
    userID <- getUser auth
    withDB $ run $ Docs.getTreeRevision userID $ TreeRevisionRef docID revision

getTextHistoryHandler
    :: AuthResult Auth.Token
    -> DocumentID
    -> TextElementID
    -> Maybe UTCTime
    -> Maybe Docs.Limit
    -> Handler TextRevisionHistory
getTextHistoryHandler auth docID textID before limit = do
    userID <- getUser auth
    withDB $
        run $
            Docs.getTextHistory
                userID
                (TextElementRef docID textID)
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
    mapErr (Docs.NoPermissionInGroup groupID) =
        err403
            { errBody =
                LBS.pack $
                    "You are not an admin in group "
                        ++ show groupID
                        ++ "!\n"
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
