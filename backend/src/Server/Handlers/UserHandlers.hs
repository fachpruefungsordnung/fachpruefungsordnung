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
import Data.Text (Text)
import DocumentManagement.Document as Document
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Servant
import Servant.Auth.Server
import Server.Auth (AuthMethod)
import qualified Server.Auth as Auth
import Server.HandlerUtil
import qualified UserManagement.DocumentPermission as Permission
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User
import Prelude hiding (readFile)

type UserAPI =
    Auth AuthMethod Auth.Token
        :> "register"
        :> ReqBody '[JSON] Auth.UserRegisterData
        :> Post '[JSON] User.UserID
        :<|> "me"
            :> ( Auth AuthMethod Auth.Token
                    :> Get '[JSON] User.FullUser
                    :<|> Auth AuthMethod Auth.Token
                        :> "documents"
                        :> Get '[JSON] [Permission.DocumentWithPermission]
                    :<|> Auth AuthMethod Auth.Token
                        :> "reset-password"
                        :> ReqBody '[JSON] Text
                        :> Patch '[JSON] NoContent
               )
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
                    :<|> Auth AuthMethod Auth.Token
                        :> Capture "userId" User.UserID
                        :> "documents"
                        :> Get '[JSON] [Permission.DocumentWithPermission]
               )

userServer :: Server UserAPI
userServer =
    registerHandler
        :<|> ( meHandler
                :<|> getMyDocumentsHandler
                :<|> updateMyPasswordHandler
             )
        :<|> getAllUsersHandler
        :<|> getUserHandler
        :<|> deleteUserHandler
        :<|> patchUserHandler
        :<|> getUsersDocumentsHandler

-- | Adds a new user to the system, if the logged in User is `Admin` or `SuperAdmin`.
--   If a groupID is given, the new user will be added
--   to this group as a `Member`.
registerHandler
    :: AuthResult Auth.Token -> Auth.UserRegisterData -> Handler User.UserID
registerHandler (Authenticated token) regData@(Auth.UserRegisterData _ _ _ mGroupID) = do
    conn <- tryGetDBConnection
    case mGroupID of
        Nothing ->
            ifSuperOrAnyAdminDo
                conn
                token
                (addNewUser conn regData)
        Just groupID ->
            ifSuperOrAdminDo
                conn
                token
                groupID
                ( addNewUser conn regData
                    >>= \userID ->
                        addRoleInGroup conn userID groupID User.Member
                            >> return userID
                )
  where
    addNewUser :: Connection -> Auth.UserRegisterData -> Handler User.UserID
    addNewUser conn (Auth.UserRegisterData {..}) = do
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
                    Left _ -> throwError errUserCreationFailed
                    Right userID -> return userID
            Right (Just _) -> throwError errEmailAlreadyUsed
            Left _ -> throwError errDatabaseAccessFailed
registerHandler _ _ = throwError errNotLoggedIn

meHandler :: AuthResult Auth.Token -> Handler User.FullUser
meHandler auth@(Authenticated Auth.Token {..}) = getUserHandler auth subject
meHandler _ = throwError errNotLoggedIn

getMyDocumentsHandler
    :: AuthResult Auth.Token -> Handler [Permission.DocumentWithPermission]
getMyDocumentsHandler auth@(Authenticated Auth.Token {..}) = getUsersDocumentsHandler auth subject
getMyDocumentsHandler _ = throwError errNotLoggedIn

getUsersDocumentsHandler
    :: AuthResult Auth.Token
    -> User.UserID
    -> Handler [Permission.DocumentWithPermission]
getUsersDocumentsHandler (Authenticated Auth.Token {..}) requestedUserID = do
    if isSuperadmin || subject == requestedUserID
        then do
            conn <- tryGetDBConnection
            eList <- liftIO $ Session.run (Sessions.getAllVisibleDocuments subject) conn
            case eList of
                Left _ -> throwError errDatabaseAccessFailed
                Right docList -> do
                    mapM
                        ( \doc -> do
                            mPerm <- checkPermission conn subject (Document.documentID doc)
                            case mPerm of
                                -- this should not happen, since the document is listed in visible documents,
                                -- so the user should have atleast read permissions
                                Nothing -> throwError errDatabaseAccessFailed
                                Just perm -> return $ Permission.DocumentWithPermission perm doc
                        )
                        docList
        else throwError errSuperAdminOnly
getUsersDocumentsHandler _ _ = throwError errNotLoggedIn

updateMyPasswordHandler :: AuthResult Auth.Token -> Text -> Handler NoContent
updateMyPasswordHandler (Authenticated Auth.Token {..}) newPassword = do
    conn <- tryGetDBConnection
    PasswordHash hashedText <- liftIO $ hashPassword $ mkPassword newPassword
    eAction <-
        liftIO $ Session.run (Sessions.updateUserPWHash subject hashedText) conn
    case eAction of
        Left _ -> throwError errDatabaseAccessFailed
        Right _ -> return NoContent
updateMyPasswordHandler _ _ = throwError errNotLoggedIn

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
                                    let roles' = [User.GroupRole group name role | (group, name, Just role) <- roles]
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
                    Right (Just user)
                        | User.userID user == userID -> patchUser conn
                        | otherwise -> throwError errEmailAlreadyUsed
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
