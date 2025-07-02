{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.UserHandlers
    ( UserAPI
    , userServer
    ) where

import Control.Monad.IO.Class
import Data.Password.Argon2
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Servant
import Servant.Auth.Server
import Server.Auth (AuthMethod)
import qualified Server.Auth as Auth
import Server.HandlerUtil
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User
import Prelude hiding (readFile)
import DocumentManagement.Document as Document ( Document )


type UserAPI =
    Auth AuthMethod Auth.Token
        :> "register"
        :> ReqBody '[JSON] Auth.UserRegisterData
        :> Post '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "me"
            :> Get '[JSON] User.FullUser
        :<|> Auth AuthMethod Auth.Token
            :> "me" :> "documents"
            :> Get '[JSON] [Document]
        :<|> "users"
            :> ( Auth AuthMethod Auth.Token
                    :> Get '[JSON] [User.User]
                    :<|> Auth AuthMethod Auth.Token
                        :> Capture "userId" User.UserID
                        :> Get '[JSON] User.FullUser
                    :<|> Auth AuthMethod Auth.Token
                        :> Capture "userId" User.UserID
                        :> Delete '[JSON] NoContent
                    :<|> Auth AuthMethod Auth.Token
                        :> Capture "userId" User.UserID
                        :> ReqBody '[JSON] Auth.UserUpdate
                        :> Patch '[JSON] NoContent
               )

userServer :: Server UserAPI
userServer =
    registerHandler
        :<|> meHandler
        :<|> getMyDocumentsHandler
        :<|> getAllUsersHandler
        :<|> getUserHandler
        :<|> deleteUserHandler
        :<|> patchUserHandler

registerHandler
    :: AuthResult Auth.Token -> Auth.UserRegisterData -> Handler NoContent
registerHandler (Authenticated token) regData@(Auth.UserRegisterData _ _ _ gID) = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token gID (addNewMember regData conn)
  where
    addNewMember :: Auth.UserRegisterData -> Connection -> Handler NoContent
    addNewMember (Auth.UserRegisterData {..}) conn = do
        eUser <- liftIO $ Session.run (Sessions.getUserByEmail registerEmail) conn
        case eUser of
            Right Nothing -> do
                PasswordHash hashedText <- liftIO $ hashPassword $ mkPassword registerPassword
                eAction <-
                    liftIO $
                        Session.run
                            ( Sessions.putUser
                                ( User.UserCreate
                                    registerName
                                    registerEmail
                                    hashedText
                                )
                            )
                            conn
                case eAction of
                    Left _ -> throwError $ err500 {errBody = "user creation failed!\n"}
                    Right userID -> do
                        addRoleInGroup conn userID groupID User.Member
                        return NoContent
            Right (Just _) -> throwError $ err409 {errBody = "a user with that email exists already."}
            Left _ -> throwError errDatabaseAccessFailed
registerHandler _ _ = throwError errNotLoggedIn

meHandler :: AuthResult Auth.Token -> Handler User.FullUser
meHandler auth@(Authenticated Auth.Token {..}) = getUserHandler auth subject
meHandler _ = throwError errNotLoggedIn

getMyDocumentsHandler
    :: AuthResult Auth.Token ->  Handler [Document]
getMyDocumentsHandler (Authenticated Auth.Token {..}) = do
    conn <- tryGetDBConnection
    eList <- liftIO $ Session.run (Sessions.getAllVisibleDocuments subject) conn
    case eList of
            Left _ -> throwError errDatabaseAccessFailed
            Right list -> return list
getMyDocumentsHandler _ = throwError errNotLoggedIn

-- | Returns a list of all users to anyone thats logged in.
getAllUsersHandler :: AuthResult Auth.Token -> Handler [User.User]
getAllUsersHandler (Authenticated _) = do
    conn <- tryGetDBConnection
    eUsers <- liftIO $ Session.run Sessions.getAllUsers conn
    case eUsers of
        Left _ -> throwError errDatabaseAccessFailed
        Right users -> return users
getAllUsersHandler _ = throwError errNotLoggedIn

getUserHandler
    :: AuthResult Auth.Token -> User.UserID -> Handler User.FullUser
getUserHandler (Authenticated Auth.Token {..}) requestedUserID = do
    if isSuperadmin || subject == requestedUserID
        then do
            conn <- tryGetDBConnection
            eAction <- liftIO $ Session.run (Sessions.getUserByID requestedUserID) conn
            case eAction of
                Left _ -> throwError errDatabaseAccessFailed
                Right Nothing -> throwError $ err404 {errBody = "user not found."}
                Right (Just User.User {..}) -> do
                    eIsSuper <- liftIO $ Session.run (Sessions.checkSuperadmin requestedUserID) conn
                    case eIsSuper of
                        Left _ -> throwError errDatabaseAccessFailed
                        Right isSuper -> do
                            eAction' <- liftIO $ Session.run (Sessions.getAllUserRoles requestedUserID) conn
                            case eAction' of
                                Left _ -> throwError errDatabaseAccessFailed
                                Right roles ->
                                    let roles' = [(group, role) | (group, Just role) <- roles]
                                     in return $ User.FullUser requestedUserID userName userEmail isSuper roles'
        else
            throwError errSuperAdminOnly
getUserHandler _ _ = throwError errNotLoggedIn

deleteUserHandler
    :: AuthResult Auth.Token -> User.UserID -> Handler NoContent
deleteUserHandler (Authenticated Auth.Token {..}) requestedUserID =
    if isSuperadmin
        then do
            conn <- tryGetDBConnection
            eAction <- liftIO $ Session.run (Sessions.deleteUser requestedUserID) conn
            case eAction of
                Left _ -> throwError errDatabaseAccessFailed
                Right _ -> return NoContent
        else
            throwError errSuperAdminOnly
deleteUserHandler _ _ = throwError errNotLoggedIn

patchUserHandler
    :: AuthResult Auth.Token -> User.UserID -> Auth.UserUpdate -> Handler NoContent
patchUserHandler (Authenticated Auth.Token {..}) userID (Auth.UserUpdate {..}) = do
    conn <- tryGetDBConnection
    if isSuperadmin || subject == userID
        then case newEmail of
            Nothing -> patchUser conn
            Just newEmail' -> do
                -- check if email is already used for some account
                eUser <- liftIO $ Session.run (Sessions.getUserByEmail newEmail') conn
                case eUser of
                    Left _ -> throwError errDatabaseAccessFailed
                    Right Nothing -> patchUser conn
                    Right _ -> throwError errEmailAlreadyUsed
        else
            throwError errSuperAdminOnly
  where
    patchUser :: Connection -> Handler NoContent
    patchUser conn = do
        updateEntry conn newName $ Sessions.updateUserName userID
        updateEntry conn newEmail $ Sessions.updateUserEmail userID
        return NoContent

    updateEntry :: Connection -> Maybe a -> (a -> Session.Session ()) -> Handler ()
    updateEntry _ Nothing _ = return ()
    updateEntry conn (Just val) upd = do
        eAction <- liftIO $ Session.run (upd val) conn
        case eAction of
            Left _ -> throwError errDatabaseAccessFailed
            Right _ -> return ()
patchUserHandler _ _ _ = throwError errNotLoggedIn
