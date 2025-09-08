{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Server.HandlerUtil
    ( ifSuperOrAdminDo
    , ifSuperOrGroupMemberDo
    , ifSuperOrAnyAdminDo
    , tryGetDBConnection
    , addRoleInGroup
    , checkPermission
    , getGroupOfDocument
    , errDatabaseConnectionFailed
    , errDatabaseAccessFailed
    , errNoMemberOfThisGroup
    , errNoAdminInAnyGroup
    , errNoAdminOfThisGroup
    , errSuperAdminOnly
    , errNotLoggedIn
    , errUserCreationFailed
    , errUserNotFound
    , errEmailAlreadyUsed
    , errDocumentDoesNotExist
    , errNoPermission
    ) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Database (getConnection)
import Docs.Document (DocumentID)
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
                        else throwError errNoAdminOfThisGroup

-- | Checks if user is Member (or Admin) in specified group or Superadmin.
--   If so, it calls the given callback Handler;
--   Otherwise, it throws a 403 error.
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

-- | Checks if user is SuperAdmin or Admin in ANY Group.
--   If so, it calss the given callback Handler;
--   Otherwise, it throws a 403 error.
ifSuperOrAnyAdminDo :: Connection -> Auth.Token -> Handler a -> Handler a
ifSuperOrAnyAdminDo conn (Auth.Token {..}) callback =
    if isSuperadmin
        then callback
        else do
            -- Check if User is Admin in ANY group
            eRoles <- liftIO $ run (Sessions.getAllUserRoles subject) conn
            case eRoles of
                Left _ -> throwError errDatabaseAccessFailed
                Right [] -> throwError errNoAdminInAnyGroup
                Right roles ->
                    if any (\(_, _, mr) -> mr == Just User.Admin) roles
                        then callback
                        else throwError errNoAdminInAnyGroup

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
--   or return external Permission. Members will always get `Editor` permission.
-- Nothing            -> both paths failed (no permission)
-- Just Permission -> User has given access rights
checkPermission
    :: Connection
    -> User.UserID
    -> DocumentID
    -> Handler (Maybe Permission.Permission)
checkPermission conn userID docID = do
    eIsMember <- liftIO $ run (Sessions.checkGroupPermission userID docID) conn
    case eIsMember of
        Left _ -> throwError errDatabaseAccessFailed
        Right True -> return $ Just Permission.Edit -- user is member of right group
        Right False -> do
            ePerm <- liftIO $ run (Sessions.getExternalPermission userID docID) conn
            case ePerm of
                Left _ -> throwError errDatabaseAccessFailed
                Right x -> return x

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
errDatabaseConnectionFailed = err500 {errBody = "\"Connection to database failed!\""}

errDatabaseAccessFailed :: ServerError
errDatabaseAccessFailed = err500 {errBody = "\"Database access failed!\""}

errFailedToSetRole :: ServerError
errFailedToSetRole = err500 {errBody = "\"Failed to set role in Database!\""}

errNoMemberOfThisGroup :: ServerError
errNoMemberOfThisGroup =
    err403
        { errBody = "\"You have to be Member of the group to perform this action!\""
        }

errNoAdminOfThisGroup :: ServerError
errNoAdminOfThisGroup =
    err403
        { errBody = "\"You have to be Admin of the group to perform this action!\""
        }

errNoAdminInAnyGroup :: ServerError
errNoAdminInAnyGroup = err403 {errBody = "\"You have to be an Admin to perform this action!\""}

errSuperAdminOnly :: ServerError
errSuperAdminOnly =
    err403 {errBody = "\"You have to be Superadmin to perform this action!\""}

errNotLoggedIn :: ServerError
errNotLoggedIn =
    err401
        { errBody = "\"Not allowed! You need to be logged in to perform this action.\""
        }

errUserCreationFailed :: ServerError
errUserCreationFailed = err500 {errBody = "\"User creation failed!\""}

errUserNotFound :: ServerError
errUserNotFound = err404 {errBody = "\"User not member of this group.\""}

errEmailAlreadyUsed :: ServerError
errEmailAlreadyUsed = err409 {errBody = "\"Email is already in use.\""}

errDocumentDoesNotExist :: ServerError
errDocumentDoesNotExist = err404 {errBody = "\"Document not found.\""}

errNoPermission :: ServerError
errNoPermission = err403 {errBody = "\"Insufficient permission to perform this action.\""}
