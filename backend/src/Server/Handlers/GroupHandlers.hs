{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.GroupHandlers
    ( GroupAPI
    , groupServer
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
import VersionControl.Commit (ExistingCommit)
import Prelude hiding (readFile)

type GroupAPI =
    Auth AuthMethod Auth.Token
        :> "groups"
        :> ReqBody '[JSON] Group.Group
        :> Post '[JSON] Group.GroupID
        :<|> Auth AuthMethod Auth.Token
            :> "groups"
            :> Capture "groupID" Group.GroupID
            :> Get '[JSON] [User.UserInfo]
        :<|> Auth AuthMethod Auth.Token
            :> "groups"
            :> Capture "groupID" Group.GroupID
            :> Delete '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "groups"
            :> Capture "groupID" Group.GroupID
            :> "documents"
            :> Get '[JSON] [ExistingCommit]

groupServer :: Server GroupAPI
groupServer =
    createGroupHandler
        :<|> groupMembersHandler
        :<|> deleteGroupHandler
        :<|> getAllGroupDocumentsHandler

groupMembersHandler
    :: AuthResult Auth.Token -> Group.GroupID -> Handler [User.UserInfo]
groupMembersHandler (Authenticated token) groupID = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID (getMembers conn)
  where
    getMembers :: Connection -> Handler [User.UserInfo]
    getMembers conn = do
        eMembers <- liftIO $ Session.run (Sessions.getMembersOfGroup groupID) conn
        case eMembers of
            Left _ -> throwError errDatabaseAccessFailed
            Right members -> return members
groupMembersHandler _ _ = throwError errNotLoggedIn

createGroupHandler
    :: AuthResult Auth.Token -> Group.Group -> Handler Group.GroupID
createGroupHandler (Authenticated Auth.Token {..}) (Group.Group {..}) = do
    conn <- tryGetDBConnection
    if isSuperadmin
        then createGroup conn
        else do
            -- Check if User is Admin in ANY group
            eRoles <- liftIO $ Session.run (Sessions.getAllUserRoles subject) conn
            case eRoles of
                Left _ -> throwError errDatabaseAccessFailed
                Right roles ->
                    if any (\(_, mr) -> mr == Just User.Admin) roles
                        then do
                            groupID <- createGroup conn
                            addRoleInGroup conn subject groupID User.Admin
                            return groupID
                        else
                            throwError $
                                err403 {errBody = "You need to be Admin of any group to perform this action!\n"}
  where
    createGroup :: Connection -> Handler Group.GroupID
    createGroup conn = do
        eGroupID <-
            liftIO $ Session.run (Sessions.addGroup groupName groupDescription) conn
        case eGroupID of
            Left _ -> throwError errDatabaseAccessFailed
            Right groupID -> return groupID
createGroupHandler _ _ = throwError errNotLoggedIn

deleteGroupHandler
    :: AuthResult Auth.Token -> Group.GroupID -> Handler NoContent
deleteGroupHandler (Authenticated token) groupID = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID (deleteGroup conn)
  where
    deleteGroup :: Connection -> Handler NoContent
    deleteGroup conn = do
        eResult <- liftIO $ Session.run (Sessions.deleteGroup groupID) conn
        case eResult of
            Left _ -> throwError errDatabaseAccessFailed
            Right () -> return NoContent
deleteGroupHandler _ _ = throwError errNotLoggedIn

getAllGroupDocumentsHandler
    :: AuthResult Auth.Token -> Group.GroupID -> Handler [ExistingCommit]
getAllGroupDocumentsHandler (Authenticated token) groupID = do
    conn <- tryGetDBConnection
    ifGroupMemberDo conn token groupID (getAllDocs groupID)
  where
    getAllDocs :: Group.GroupID -> Handler [ExistingCommit]
    getAllDocs groupID = undefined -- TODO: function call for collecting all docs of given group
getAllGroupDocumentsHandler _ _ = throwError errNotLoggedIn
