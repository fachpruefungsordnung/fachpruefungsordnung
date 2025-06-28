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
import Data.Text (Text)
import Data.Vector (Vector)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Servant
import Servant.Auth.Server
import Server.Auth (AuthMethod)
import qualified Server.Auth as Auth
import Server.HandlerUtil
import Server.Handlers.RenderHandlers (RenderAPI, renderServer)
import qualified UserManagement.DocumentPermission as Permission
import qualified UserManagement.Group as Group
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User
import VersionControl
import VersionControl.Commit
import VersionControl.Document as Document
import Prelude hiding (readFile)

type DocumentAPI =
    "documents"
        :> ( Auth AuthMethod Auth.Token
                :> Capture "documentID" Document.DocumentID
                :> Get '[JSON] Document
                :<|> Auth AuthMethod Auth.Token
                    :> ReqBody '[JSON] (Text, Group.GroupID)
                    :> Post '[JSON] Document
                :<|> Auth AuthMethod Auth.Token
                    :> Capture "documentID" Document.DocumentID
                    :> Delete '[JSON] NoContent
                :<|> "commits"
                    :> ( Auth AuthMethod Auth.Token
                            :> Capture "documentID" Document.DocumentID
                            :> Get '[JSON] (Vector ExistingCommit)
                            :<|> Auth AuthMethod Auth.Token
                                :> Capture "documentID" Document.DocumentID
                                :> ReqBody '[JSON] CreateCommit
                                :> Post '[JSON] Document
                            :<|> Auth AuthMethod Auth.Token
                                :> Capture "documentID" Document.DocumentID
                                :> Capture "commitID" CommitID
                                :> Get '[JSON] ExistingCommit
                       )
                :<|> "external"
                    :> ( Auth AuthMethod Auth.Token
                            :> Capture "documentID" Document.DocumentID
                            :> Get '[JSON] [(User.UserID, Permission.DocPermission)]
                            :<|> Auth AuthMethod Auth.Token
                                :> Capture "documentID" Document.DocumentID
                                :> Capture "userID" User.UserID
                                :> Get '[JSON] (Maybe Permission.DocPermission)
                            :<|> Auth AuthMethod Auth.Token
                                :> Capture "documentID" Document.DocumentID
                                :> Capture "userID" User.UserID
                                :> ReqBody '[JSON] Permission.DocPermission
                                :> Put '[JSON] NoContent
                            :<|> Auth AuthMethod Auth.Token
                                :> Capture "documentID" Document.DocumentID
                                :> Capture "userID" User.UserID
                                :> Delete '[JSON] NoContent
                       )
                :<|> RenderAPI
           )

documentServer :: Server DocumentAPI
documentServer =
    getDocumentHandler
        :<|> postDocumentHandler
        :<|> deleteDocumentHandler
        :<|> ( getAllCommitsHandler
                :<|> postCommitHandler
                :<|> getCommitHandler
             )
        :<|> ( getAllExternalUsersDocumentHandler
                :<|> getExternalUserDocumentHandler
                :<|> putExternalUserDocumentHandler
                :<|> deleteExternalUserDocumentHandler
             )
        :<|> renderServer

getDocumentHandler
    :: AuthResult Auth.Token -> Document.DocumentID -> Handler Document
getDocumentHandler (Authenticated Auth.Token {..}) docID = do
    conn <- tryGetDBConnection
    mPerm <- checkDocPermission conn subject docID
    case mPerm of
        Nothing -> throwError errNoPermission
        Just perm ->
            if Permission.hasPermission perm Permission.Read
                then do
                    eDocument <- liftIO $ getDocument docID (Context conn)
                    case eDocument of
                        Left _ -> throwError errDatabaseAccessFailed
                        Right doc -> return doc
                else throwError errNoPermission
getDocumentHandler _ _ = throwError errNotLoggedIn

postDocumentHandler
    :: AuthResult Auth.Token -> (Text, Group.GroupID) -> Handler Document
postDocumentHandler (Authenticated token) (name, groupID) = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID (postDocument conn)
  where
    postDocument :: Connection -> Handler Document
    postDocument conn = do
        eAction <- liftIO $ createDocument name groupID (Context conn)
        case eAction of
            Left _ -> throwError errDatabaseAccessFailed
            Right doc -> return doc
postDocumentHandler _ _ = throwError errNotLoggedIn

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

getAllCommitsHandler
    :: AuthResult Auth.Token -> Document.DocumentID -> Handler (Vector ExistingCommit)
getAllCommitsHandler (Authenticated Auth.Token {..}) docID = do
    conn <- tryGetDBConnection
    mPerm <- checkDocPermission conn subject docID
    case mPerm of
        Nothing -> throwError errNoPermission
        Just perm ->
            if Permission.hasPermission perm Permission.Read
                then do
                    eVector <- liftIO $ getCommitGraph docID (Context conn)
                    case eVector of
                        Left _ -> throwError errDatabaseAccessFailed
                        Right vec -> return vec
                else throwError errNoPermission
getAllCommitsHandler _ _ = throwError errNotLoggedIn

postCommitHandler
    :: AuthResult Auth.Token -> Document.DocumentID -> CreateCommit -> Handler Document
postCommitHandler (Authenticated Auth.Token {..}) docID cc = do
    conn <- tryGetDBConnection
    mPerm <- checkDocPermission conn subject docID
    case mPerm of
        Nothing -> throwError errNoPermission
        Just perm ->
            if Permission.hasPermission perm Permission.Edit
                then do
                    eDocument <- liftIO $ createDocumentCommit docID cc (Context conn)
                    case eDocument of
                        Left _ -> throwError errDatabaseAccessFailed
                        Right doc -> return doc
                else throwError errNoPermission
postCommitHandler _ _ _ = throwError errNotLoggedIn

getCommitHandler
    :: AuthResult Auth.Token
    -> Document.DocumentID
    -> CommitID
    -> Handler ExistingCommit
getCommitHandler (Authenticated Auth.Token {..}) docID commitID = do
    conn <- tryGetDBConnection
    mPerm <- checkDocPermission conn subject docID
    case mPerm of
        Nothing -> throwError errNoPermission
        Just perm ->
            if Permission.hasPermission perm Permission.Edit
                then do
                    eCommit <- liftIO $ getCommit commitID (Context conn)
                    case eCommit of
                        Left _ -> throwError errDatabaseAccessFailed
                        Right com -> return com
                else throwError errNoPermission
getCommitHandler _ _ _ = throwError errNotLoggedIn

getAllExternalUsersDocumentHandler
    :: AuthResult Auth.Token
    -> Document.DocumentID
    -> Handler [(User.UserID, Permission.DocPermission)]
getAllExternalUsersDocumentHandler (Authenticated token) docID = do
    conn <- tryGetDBConnection
    groupID <- getGroupOfDocument conn docID
    ifSuperOrAdminDo conn token groupID (getUsers conn)
  where
    getUsers :: Connection -> Handler [(User.UserID, Permission.DocPermission)]
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
    -> Handler (Maybe Permission.DocPermission)
getExternalUserDocumentHandler (Authenticated token) docID userID = do
    conn <- tryGetDBConnection
    groupID <- getGroupOfDocument conn docID
    ifSuperOrAdminDo conn token groupID (getUser conn)
  where
    getUser :: Connection -> Handler (Maybe Permission.DocPermission)
    getUser conn = do
        emPermission <-
            liftIO $ Session.run (Sessions.getExternalDocPermission userID docID) conn
        case emPermission of
            Left _ -> throwError errDatabaseAccessFailed
            Right mPermission -> return mPermission
getExternalUserDocumentHandler _ _ _ = throwError errNotLoggedIn

putExternalUserDocumentHandler
    :: AuthResult Auth.Token
    -> Document.DocumentID
    -> User.UserID
    -> Permission.DocPermission
    -> Handler NoContent
putExternalUserDocumentHandler (Authenticated token) docID userID perm = do
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
putExternalUserDocumentHandler _ _ _ _ = throwError errNotLoggedIn

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
