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
import qualified DocumentManagement.Document as Document
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

type GroupAPI =
    "groups"
        :> ( Auth AuthMethod Auth.Token
                :> ReqBody '[JSON] Group.GroupCreate
                :> Post '[JSON] Group.GroupID
                :<|> Auth AuthMethod Auth.Token
                    :> Get '[JSON] [Group.GroupOverview]
                :<|> Auth AuthMethod Auth.Token
                    :> Capture "groupID" Group.GroupID
                    :> Get '[JSON] Group.Group
                :<|> Auth AuthMethod Auth.Token
                    :> Capture "groupID" Group.GroupID
                    :> Delete '[JSON] NoContent
                :<|> Auth AuthMethod Auth.Token
                    :> Capture "groupID" Group.GroupID
                    :> "documents"
                    :> Get '[JSON] [Document.Document]
           )

groupServer :: Server GroupAPI
groupServer =
    createGroupHandler
        :<|> getAllGroupsHandler
        :<|> getGroupHandler
        :<|> deleteGroupHandler
        :<|> getAllGroupDocumentsHandler

createGroupHandler
    :: AuthResult Auth.Token -> Group.GroupCreate -> Handler Group.GroupID
createGroupHandler (Authenticated token@Auth.Token {..}) (Group.GroupCreate {..}) = do
    conn <- tryGetDBConnection
    ifSuperOrAnyAdminDo conn token (createGroupAndAddAdmin conn)
  where
    createGroupAndAddAdmin :: Connection -> Handler Group.GroupID
    createGroupAndAddAdmin conn = do
        eBool <-
            liftIO $ Session.run (Sessions.checkGroupNameExistence groupCreateName) conn
        case eBool of
            Left _ -> throwError errDatabaseAccessFailed
            Right True -> throwError $ err409 {errBody = "A group with that name exists already."}
            Right False -> do
                eGroupID <-
                    liftIO $
                        Session.run (Sessions.addGroup groupCreateName groupCreateDescription) conn
                case eGroupID of
                    Left _ -> throwError errDatabaseAccessFailed
                    Right groupID ->
                        addRoleInGroup conn subject groupID User.Admin >> return groupID
createGroupHandler _ _ = throwError errNotLoggedIn

-- | If the logged in user is SuperAdmin returns list of all existing groups as
--   [(GroupID, GroupName)]
getAllGroupsHandler :: AuthResult Auth.Token -> Handler [Group.GroupOverview]
getAllGroupsHandler (Authenticated Auth.Token {..}) =
    if isSuperadmin
        then do
            conn <- tryGetDBConnection
            eGroups <- liftIO $ Session.run Sessions.getAllGroupsOverview conn
            case eGroups of
                Left _ -> throwError errDatabaseAccessFailed
                Right groups -> return groups
        else throwError errSuperAdminOnly
getAllGroupsHandler _ = throwError errNotLoggedIn

getGroupHandler
    :: AuthResult Auth.Token -> Group.GroupID -> Handler Group.Group
getGroupHandler (Authenticated token) groupID = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID (getGroup conn)
  where
    getGroup :: Connection -> Handler Group.Group
    getGroup conn = do
        eGroupInfo <- liftIO $ Session.run (Sessions.getGroupInfo groupID) conn
        case eGroupInfo of
            Left _ -> throwError errDatabaseAccessFailed
            Right (Group.GroupCreate name mDesc) -> do
                members <- getMembers conn
                return $ Group.Group groupID name mDesc members

    getMembers :: Connection -> Handler [User.UserInfo]
    getMembers conn = do
        eMembers <- liftIO $ Session.run (Sessions.getMembersOfGroup groupID) conn
        case eMembers of
            Left _ -> throwError errDatabaseAccessFailed
            Right members -> return members
getGroupHandler _ _ = throwError errNotLoggedIn

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
    :: AuthResult Auth.Token -> Group.GroupID -> Handler [Document.Document]
getAllGroupDocumentsHandler (Authenticated token) groupID = do
    conn <- tryGetDBConnection
    ifSuperOrGroupMemberDo conn token groupID (getAllDocs conn)
  where
    getAllDocs :: Connection -> Handler [Document.Document]
    getAllDocs conn = do
        eList <- liftIO $ Session.run (Sessions.getAllDocumentsOfGroup groupID) conn
        case eList of
            Left _ -> throwError errDatabaseAccessFailed
            Right list -> return list
getAllGroupDocumentsHandler _ _ = throwError errNotLoggedIn
