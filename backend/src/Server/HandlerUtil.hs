{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server.HandlerUtil
    ( ifSuperOrAdminDo
    , ifSuperOrGroupMemberDo
    , tryGetDBConnection
    , addRoleInGroup
    , checkDocPermission
    , getGroupOfDocument
    , errDatabaseConnectionFailed
    , errDatabaseAccessFailed
    , errNoMemberOfThisGroup
    , errNoAdminOfThisGroup
    , errSuperAdminOnly
    , errNotLoggedIn
    , errUserNotFound
    , errEmailAlreadyUsed
    , errDocumentDoesNotExist
    , errNoPermission
    ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Database (getConnection)
import DocumentManagement.Document (DocumentID)
import Hasql.Connection (Connection)
import Hasql.Session (run)
import Servant
import qualified Server.Auth as Auth
import qualified UserManagement.DocumentPermission as Permission
import qualified UserManagement.Group as Group
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User

-- | Checks if User is SuperAdmin or Admin in the given group.
--   If so, it calls the given callback Handler;
--   Otherwise, it throws a 403 error.
ifSuperOrAdminDo
    :: Connection -> Auth.Token -> Group.GroupID -> Handler a -> Handler a
ifSuperOrAdminDo conn (Auth.Token {..}) groupID callback =
    if isSuperadmin
        then callback
        else do
            emRole <- liftIO $ run (Sessions.getUserRoleInGroup subject groupID) conn
            case emRole of
                Left _ -> throwError errDatabaseAccessFailed
                Right Nothing ->
                    throwError errNoAdminOfThisGroup
                Right (Just role) ->
                    if role == User.Admin
                        then callback
                        else
                            throwError errNoAdminOfThisGroup

-- | Checks if user is Member (or Admin) in specified group or Superadmin.
--   If so, it calls the given callback Handler;
-- Otherwise, it throws a 403 error.
ifSuperOrGroupMemberDo
    :: Connection -> Auth.Token -> Group.GroupID -> Handler a -> Handler a
ifSuperOrGroupMemberDo conn (Auth.Token {..}) groupID callback = do
    if isSuperadmin
        then callback
        else do
            eMembership <- liftIO $ run (Sessions.checkGroupMembership subject groupID) conn
            case eMembership of
                Left _ -> throwError errDatabaseAccessFailed
                Right False -> throwError errNoMemberOfThisGroup
                Right True -> callback

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

-- | Check if User is Member (or Admin) of the group that owns the specified document
--   or return external DocPermission. Members will always get `Editor` permission.
-- Nothing            -> both paths failed (no permission)
-- Just DocPermission -> User has given access rights
checkDocPermission
    :: Connection
    -> User.UserID
    -> DocumentID
    -> Handler (Maybe Permission.DocPermission)
checkDocPermission conn userID docID = do
    eIsMember <- liftIO $ run (Sessions.checkGroupDocPermission userID docID) conn
    case eIsMember of
        Left _ -> throwError errDatabaseAccessFailed
        Right True -> return $ Just Permission.Editor -- user is member of right group
        Right False -> do
            ePerm <- liftIO $ run (Sessions.getExternalDocPermission userID docID) conn
            case ePerm of
                Left _ -> throwError errDatabaseAccessFailed
                Right Nothing -> return Nothing
                Right (Just perm) -> return $ Just perm

-- | Get the groupID of the group that owns the specified document
getGroupOfDocument :: Connection -> DocumentID -> Handler Group.GroupID
getGroupOfDocument conn docID = do
    emgroupID <- liftIO $ run (Sessions.getDocumentGroupID docID) conn
    case emgroupID of
        Left _ -> throwError errDatabaseAccessFailed
        Right Nothing -> throwError errDocumentDoesNotExist
        Right (Just groupID) -> return groupID

-- Specific errors
errDatabaseConnectionFailed :: ServerError
errDatabaseConnectionFailed = err500 {errBody = "Connection to database failed!\n"}

errDatabaseAccessFailed :: ServerError
errDatabaseAccessFailed = err500 {errBody = "Database access failed!\n"}

errFailedToSetRole :: ServerError
errFailedToSetRole = err500 {errBody = "Failed to set role in Database!"}

errNoMemberOfThisGroup :: ServerError
errNoMemberOfThisGroup =
    err403
        { errBody = "You have to be Member of the group to perform this action!\n"
        }

errNoAdminOfThisGroup :: ServerError
errNoAdminOfThisGroup =
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

errEmailAlreadyUsed :: ServerError
errEmailAlreadyUsed = err409 {errBody = "Email is already in use."}

errDocumentDoesNotExist :: ServerError
errDocumentDoesNotExist = err404 {errBody = "Document not found."}

errNoPermission :: ServerError
errNoPermission = err403 {errBody = "Insufficient permission to perform this action."}
