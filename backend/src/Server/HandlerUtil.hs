{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server.HandlerUtil
    ( ifSuperOrAdminDo
    , tryGetDBConnection
    , addRoleInGroup
    , errDatabaseConnectionFailed
    , errDatabaseAccessFailed
    , errNoAdminInThisGroup
    , errSuperAdminOnly
    , errIsAlreadySuperadmin
    , errNotLoggedIn
    , errUserNotFound
    ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Database (getConnection)
import Hasql.Connection (Connection)
import Hasql.Session (run)
import Servant
import Server.Auth (Token (..))
import UserManagement.Group as Group
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User

-- | Checks if User is SuperAdmin or Admin in the given group.
--   If so, it calls the given callback Handler;
--   Otherwise, it throws a 403 error.
ifSuperOrAdminDo :: Connection -> Token -> GroupID -> Handler a -> Handler a
ifSuperOrAdminDo conn (Token {..}) groupID callback =
    if isSuperadmin
        then callback
        else do
            emRole <- liftIO $ run (Sessions.getUserRoleInGroup subject groupID) conn
            case emRole of
                Left _ -> throwError errDatabaseAccessFailed
                Right Nothing ->
                    throwError errNoAdminInThisGroup
                Right (Just role) ->
                    if role == User.Admin
                        then callback
                        else
                            throwError errNoAdminInThisGroup

-- | Gets DB Connection and throws 500 error if it fails
tryGetDBConnection :: Handler Connection
tryGetDBConnection = do
    eConn <- liftIO getConnection
    case eConn of
        Left _ -> throwError errDatabaseConnectionFailed
        Right conn -> return conn

-- | Adds given role in group to User
addRoleInGroup
    :: Connection -> User.UserID -> Group.GroupID -> User.Role -> Handler ()
addRoleInGroup conn userID groupID role = do
    eResult <- liftIO $ run (Sessions.addRole userID groupID role) conn
    case eResult of
        Right () -> return ()
        Left _ -> throwError errFailedToSetRole

-- Specific errors
errDatabaseConnectionFailed :: ServerError
errDatabaseConnectionFailed = err500 {errBody = "Connection to database failed!\n"}

errDatabaseAccessFailed :: ServerError
errDatabaseAccessFailed = err500 {errBody = "Database access failed!\n"}

errFailedToSetRole :: ServerError
errFailedToSetRole = err500 {errBody = "Failed to set role in Database!"}

errNoAdminInThisGroup :: ServerError
errNoAdminInThisGroup =
    err403 {errBody = "You have to be Admin of the group to perform this action!\n"}

errSuperAdminOnly :: ServerError
errSuperAdminOnly =
    err403 {errBody = "You have to be Superadmin to perform this action!\n"}

errNotLoggedIn :: ServerError
errNotLoggedIn =
    err401
        { errBody = "Not allowed! You need to be logged in to perform this action.\n"
        }

errUserNotFound :: ServerError
errUserNotFound = err404 {errBody = "User not member of this group."}

errIsAlreadySuperadmin :: ServerError
errIsAlreadySuperadmin = err409 {errBody = "User already has Superadmin privileges."}