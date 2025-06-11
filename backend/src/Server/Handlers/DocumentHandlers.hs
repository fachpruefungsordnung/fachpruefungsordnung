{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.DocumentHandlers
    ( DocumentAPI
    , documentServer
    ) where

import Control.Monad.IO.Class
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Servant
import Servant.Auth.Server
import Server.Auth (AuthMethod)
import qualified Server.Auth as Auth
import Server.HandlerUtil
import qualified UserManagement.Document as Document
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User
import VersionControl.Commit
import Prelude hiding (readFile)

type DocumentAPI =
    Auth AuthMethod Auth.Token
        :> "documents"
        :> Capture "documentID" Document.DocumentID
        :> Get '[JSON] ExistingCommit
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> Delete '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> "external"
            :> Get '[JSON] [(User.UserID, Document.DocPermission)]
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> "external"
            :> Capture "userID" User.UserID
            :> Get '[JSON] (Maybe Document.DocPermission)
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> "external"
            :> Capture "userID" User.UserID
            :> ReqBody '[JSON] Document.DocPermission
            :> Post '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> "external"
            :> Capture "userID" User.UserID
            :> Delete '[JSON] NoContent

documentServer :: Server DocumentAPI
documentServer =
    getDocumentHandler
        :<|> deleteDocumentHandler
        :<|> getAllExternalUsersDocumentHandler
        :<|> getExternalUserDocumentHandler
        :<|> postExternalUserDocumentHandler
        :<|> deleteExternalUserDocumentHandler

getDocumentHandler
    :: AuthResult Auth.Token -> Document.DocumentID -> Handler ExistingCommit
getDocumentHandler (Authenticated Auth.Token {..}) docID = do
    conn <- tryGetDBConnection
    mPerm <- checkDocPermission conn subject docID
    case mPerm of
        Nothing -> throwError errNoPermission
        Just perm ->
            if perm >= Document.Read
                then undefined -- TODO: function for returning doc
                else throwError errNoPermission
getDocumentHandler _ _ = throwError errNotLoggedIn

deleteDocumentHandler
    :: AuthResult Auth.Token -> Document.DocumentID -> Handler NoContent
deleteDocumentHandler (Authenticated token) docID = do
    conn <- tryGetDBConnection
    groupID <- getGroupOfDocument conn docID
    ifSuperOrAdminDo conn token groupID (deleteDoc docID)
  where
    deleteDoc :: Document.DocumentID -> Handler NoContent
    deleteDoc = undefined -- TODO: function call to delete document
deleteDocumentHandler _ _ = throwError errNotLoggedIn

getAllExternalUsersDocumentHandler
    :: AuthResult Auth.Token
    -> Document.DocumentID
    -> Handler [(User.UserID, Document.DocPermission)]
getAllExternalUsersDocumentHandler (Authenticated token) docID = do
    conn <- tryGetDBConnection
    groupID <- getGroupOfDocument conn docID
    ifSuperOrAdminDo conn token groupID (getUsers conn)
  where
    getUsers :: Connection -> Handler [(User.UserID, Document.DocPermission)]
    getUsers conn = do
        eUserlist <-
            liftIO $ Session.run (Sessions.getAllExternalUsersOfDocument docID) conn
        case eUserlist of
            Left _ -> throwError errDatabaseAccessFailed
            Right userList -> return userList
getAllExternalUsersDocumentHandler _ _ = throwError errNotLoggedIn

getExternalUserDocumentHandler
    :: AuthResult Auth.Token
    -> Document.DocumentID
    -> User.UserID
    -> Handler (Maybe Document.DocPermission)
getExternalUserDocumentHandler (Authenticated token) docID userID = do
    conn <- tryGetDBConnection
    groupID <- getGroupOfDocument conn docID
    ifSuperOrAdminDo conn token groupID (getUser conn)
  where
    getUser :: Connection -> Handler (Maybe Document.DocPermission)
    getUser conn = do
        emPermission <-
            liftIO $ Session.run (Sessions.getExternalDocPermission userID docID) conn
        case emPermission of
            Left _ -> throwError errDatabaseAccessFailed
            Right mPermission -> return mPermission
getExternalUserDocumentHandler _ _ _ = throwError errNotLoggedIn

postExternalUserDocumentHandler
    :: AuthResult Auth.Token
    -> Document.DocumentID
    -> User.UserID
    -> Document.DocPermission
    -> Handler NoContent
postExternalUserDocumentHandler (Authenticated token) docID userID perm = do
    conn <- tryGetDBConnection
    groupID <- getGroupOfDocument conn docID
    ifSuperOrAdminDo conn token groupID (postUser conn)
  where
    postUser :: Connection -> Handler NoContent
    postUser conn = do
        emPermission <-
            liftIO $ Session.run (Sessions.getExternalDocPermission userID docID) conn
        case emPermission of
            Left _ -> throwError errDatabaseAccessFailed
            Right Nothing -> do
                eAction <-
                    liftIO $ Session.run (Sessions.addExternalDocPermission userID docID perm) conn
                case eAction of
                    Left _ -> throwError errDatabaseAccessFailed
                    Right _ -> return NoContent
            Right (Just _) -> do
                eAction <-
                    liftIO $
                        Session.run (Sessions.updateExternalDocPermission userID docID perm) conn
                case eAction of
                    Left _ -> throwError errDatabaseAccessFailed
                    Right _ -> return NoContent
postExternalUserDocumentHandler _ _ _ _ = throwError errNotLoggedIn

deleteExternalUserDocumentHandler
    :: AuthResult Auth.Token
    -> Document.DocumentID
    -> User.UserID
    -> Handler NoContent
deleteExternalUserDocumentHandler (Authenticated token) docID userID = do
    conn <- tryGetDBConnection
    groupID <- getGroupOfDocument conn docID
    ifSuperOrAdminDo conn token groupID (postUser conn)
  where
    postUser :: Connection -> Handler NoContent
    postUser conn = do
        eAction <-
            liftIO $ Session.run (Sessions.deleteExternalDocPermission userID docID) conn
        case eAction of
            Left _ -> throwError errDatabaseAccessFailed
            Right _ -> return NoContent
deleteExternalUserDocumentHandler _ _ _ = throwError errNotLoggedIn
