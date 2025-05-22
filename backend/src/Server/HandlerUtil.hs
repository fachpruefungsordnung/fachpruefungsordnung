{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server.HandlerUtil
    ( ifSuperOrAdminDo
    , tryGetDBConnection
    , errDatabaseConnectionFailed
    , errDatabaseAccessFailed
    , errNoAdminInThisGroup
    , errNotLoggedIn
    ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Int (Int32)
import Database (getConnection)
import Hasql.Connection (Connection)
import Hasql.Session (run)
import Servant
import Server.Auth (Token (..))
import qualified UserManagement.Sessions as Sessions
import UserManagement.User (Role (..))

{- | Checks if User is SuperAdmin or Admin in the given group.
  If so, it calls the given callback Handler;
  Otherwise, it throws a 403 error.
-}
ifSuperOrAdminDo :: Connection -> Token -> Int32 -> Handler a -> Handler a
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
                    if role == Admin
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

-- Specific errors
errDatabaseConnectionFailed :: ServerError
errDatabaseConnectionFailed = err500 {errBody = "Connection to database failed!\n"}

errDatabaseAccessFailed :: ServerError
errDatabaseAccessFailed = err500 {errBody = "Database access failed!\n"}

errNoAdminInThisGroup :: ServerError
errNoAdminInThisGroup =
    err403 {errBody = "You have to be Admin of the group to perform this action!\n"}

errNotLoggedIn :: ServerError
errNotLoggedIn =
    err401 {errBody = "Not allowed! You need to login to perform this action.\n"}