{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.UserHandlers 
    ( userHandler
    , loginHandler
    , logoutHandler
    , registerHandler
    , getUserHandler
    , deleteUserHandler
    , patchUserHandler) where

import Control.Monad.IO.Class
import Data.Password.Argon2
import Data.Vector (toList)
import Database (getConnection)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Servant
import Servant.Auth.Server
import qualified Server.Auth as Auth
import Server.HandlerUtil
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User
import Prelude hiding (readFile)


userHandler :: Handler [User.User]
userHandler = liftIO $ do
    Right connection <- getConnection
    Right vector <- Session.run Sessions.getUsers connection
    return $ toList vector

loginHandler
    :: CookieSettings
    -> JWTSettings
    -> Auth.UserLoginData
    -> Handler
        ( Headers
            '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
            NoContent
        )
loginHandler cookieSett jwtSett Auth.UserLoginData {..} = do
    conn <- tryGetDBConnection
    eUser <- liftIO $ Session.run (Sessions.getLoginRequirements loginEmail) conn
    case eUser of
        Right (Just (uid, pwhash)) -> do
            let passwordCheck = checkPassword (mkPassword loginPassword) (PasswordHash pwhash)
            case passwordCheck of
                PasswordCheckFail -> throwError $ err401 {errBody = "email or password incorrect\n"}
                PasswordCheckSuccess -> do
                    eSuperadmin <- liftIO $ Session.run (Sessions.checkSuperadmin uid) conn
                    case eSuperadmin of
                        Left _ -> throwError errDatabaseAccessFailed
                        Right isSuperadmin -> do
                            mLoginAccepted <-
                                liftIO $ acceptLogin cookieSett jwtSett (Auth.Token uid isSuperadmin)
                            case mLoginAccepted of
                                Nothing -> throwError $ err401 {errBody = "login failed! Please try again!\n"}
                                Just addHeaders -> return $ addHeaders NoContent
        Right Nothing -> throwError errUserNotFound
        Left _ -> throwError errDatabaseAccessFailed

logoutHandler
    :: CookieSettings
    -> Handler
        ( Headers
            '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
            NoContent
        )
logoutHandler cookieSett = return $ clearSession cookieSett NoContent

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
                                ( User.User
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