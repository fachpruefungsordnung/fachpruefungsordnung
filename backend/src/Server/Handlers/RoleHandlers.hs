{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.RoleHandlers
    ( RoleAPI
    , roleServer
    ) where

import Control.Monad.IO.Class
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Servant
import Servant.Auth.Server
import Server.Auth (AuthMethod)
import qualified Server.Auth as Auth
import Server.HandlerUtil
import qualified UserManagement.Group as Group
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User
import Prelude hiding (readFile)

type RoleAPI =
    "roles"
        :> ( Auth AuthMethod Auth.Token
                :> Capture "groupID" Group.GroupID
                :> Capture "userId" User.UserID
                :> Get '[JSON] User.Role
                :<|> Auth AuthMethod Auth.Token
                    :> Capture "groupID" Group.GroupID
                    :> Capture "userId" User.UserID
                    :> ReqBody '[JSON] User.Role
                    :> Put '[JSON] NoContent
                :<|> Auth AuthMethod Auth.Token
                    :> Capture "groupID" Group.GroupID
                    :> Capture "userId" User.UserID
                    :> Delete '[JSON] NoContent
                :<|> Auth AuthMethod Auth.Token
                    :> "superadmin"
                    :> Capture "userId" User.UserID
                    :> Put '[JSON] NoContent
                :<|> Auth AuthMethod Auth.Token
                    :> "superadmin"
                    :> Capture "userId" User.UserID
                    :> Delete '[JSON] NoContent
           )

roleServer :: Server RoleAPI
roleServer =
    getRoleHandler
        :<|> postRoleHandler
        :<|> deleteRoleHandler
        :<|> putSuperadminHandler
        :<|> deleteSuperadminHandler

getRoleHandler
    :: AuthResult Auth.Token -> Group.GroupID -> User.UserID -> Handler User.Role
getRoleHandler (Authenticated token) groupID userID = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID (getRole conn)
  where
    getRole :: Connection -> Handler User.Role
    getRole conn = do
        eResult <-
            liftIO $ Session.run (Sessions.getUserRoleInGroup userID groupID) conn
        case eResult of
            Left _ -> throwError errDatabaseAccessFailed
            Right (Just role) -> return role
            Right Nothing -> throwError errUserNotFound
getRoleHandler _ _ _ = throwError errNotLoggedIn

postRoleHandler
    :: AuthResult Auth.Token
    -> Group.GroupID
    -> User.UserID
    -> User.Role
    -> Handler NoContent
postRoleHandler (Authenticated token) groupID userID userRole = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID (postRole conn)
  where
    postRole :: Connection -> Handler NoContent
    postRole conn = do
        eResult <-
            liftIO $ Session.run (Sessions.getUserRoleInGroup userID groupID) conn
        case eResult of
            Left _ -> throwError errDatabaseAccessFailed
            Right (Just role) ->
                if role == userRole
                    then return NoContent
                    else do
                        eAction <-
                            liftIO $
                                Session.run (Sessions.updateUserRoleInGroup userID groupID userRole) conn
                        case eAction of
                            Left _ -> throwError errDatabaseAccessFailed
                            Right _ -> return NoContent
            Right Nothing -> do
                eAction <- liftIO $ Session.run (Sessions.addRole userID groupID userRole) conn
                case eAction of
                    Left _ -> throwError errDatabaseAccessFailed
                    Right _ -> return NoContent
postRoleHandler _ _ _ _ = throwError errNotLoggedIn

deleteRoleHandler
    :: AuthResult Auth.Token -> Group.GroupID -> User.UserID -> Handler NoContent
deleteRoleHandler (Authenticated token) groupID userID = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID (deleteRole conn)
  where
    deleteRole :: Connection -> Handler NoContent
    deleteRole conn = do
        eResult <-
            liftIO $ Session.run (Sessions.removeUserFromGroup userID groupID) conn
        case eResult of
            Left _ -> throwError errDatabaseAccessFailed
            Right _ -> return NoContent
deleteRoleHandler _ _ _ = throwError errNotLoggedIn

putSuperadminHandler
    :: AuthResult Auth.Token -> User.UserID -> Handler NoContent
putSuperadminHandler (Authenticated Auth.Token {..}) userID =
    if isSuperadmin
        then do
            conn <- tryGetDBConnection
            eIsSuper <- liftIO $ Session.run (Sessions.checkSuperadmin userID) conn
            case eIsSuper of
                Left _ -> throwError errDatabaseAccessFailed
                Right True -> return NoContent -- user already is SuperAdmin
                Right False -> do
                    eAction <- liftIO $ Session.run (Sessions.addSuperadmin userID) conn
                    case eAction of
                        Left _ -> throwError errDatabaseAccessFailed
                        Right _ -> return NoContent
        else throwError errSuperAdminOnly
putSuperadminHandler _ _ = throwError errNotLoggedIn

deleteSuperadminHandler
    :: AuthResult Auth.Token -> User.UserID -> Handler NoContent
deleteSuperadminHandler (Authenticated Auth.Token {..}) userID =
    if isSuperadmin
        then do
            conn <- tryGetDBConnection
            eAction <- liftIO $ Session.run (Sessions.removeSuperadmin userID) conn
            case eAction of
                Left _ -> throwError errDatabaseAccessFailed
                Right _ -> return NoContent
        else throwError errSuperAdminOnly
deleteSuperadminHandler _ _ = throwError errNotLoggedIn
