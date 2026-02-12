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

import Control.Monad (forM_, when)
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

import Data.Containers.ListUtils (nubOrd)
import Data.Maybe (isNothing)
import UserManagement.Group (GroupPatch (patchDescription))
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
                    :> ReqBody '[JSON] Group.GroupPatch
                    :> Patch '[JSON] Group.GroupOverview
           )

groupServer :: Server GroupAPI
groupServer =
    createGroupHandler
        :<|> getAllGroupsHandler
        :<|> getGroupHandler
        :<|> deleteGroupHandler
        :<|> patchGroupHandler

-- | Creates 'Group', adds sender as 'Admin' and all users listed as 'Member';
--   If adding any user fails, the group is still created
createGroupHandler
    :: AuthResult Auth.Token -> Group.GroupCreate -> Handler Group.GroupID
createGroupHandler (Authenticated token@Auth.Token {..}) (Group.GroupCreate {..}) = do
    conn <- tryGetDBConnection
    ifSuperOrAnyAdminDo conn token $ do
        exists <- runDB conn $ Sessions.checkGroupNameExistence groupCreateName
        when exists $
            throwError $
                err409 {errBody = "\"A group with that name exists already.\""}
        groupID <- runDB conn $ Sessions.addGroup groupCreateName groupCreateDescription
        addRoleInGroup conn subject groupID User.Admin
        forM_ groupCreateUsers $ \users ->
            forM_ (nubOrd $ filter (/= subject) users) $ \user ->
                addRoleInGroup conn user groupID User.Member
        return groupID
createGroupHandler _ _ = throwError errNotLoggedIn

-- | If the logged in user is SuperAdmin returns list of all existing groups as
--   [(GroupID, GroupName)]
getAllGroupsHandler :: AuthResult Auth.Token -> Handler [Group.GroupOverview]
getAllGroupsHandler (Authenticated Auth.Token {..}) = do
    guardSuperAdmin isSuperadmin
    conn <- tryGetDBConnection
    runDB conn Sessions.getAllGroupsOverview
getAllGroupsHandler _ = throwError errNotLoggedIn

getGroupHandler
    :: AuthResult Auth.Token -> Group.GroupID -> Handler Group.Group
getGroupHandler (Authenticated token) groupID = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID $ do
        Group.GroupOverview _ name mDesc <- runDB conn $ Sessions.getGroupInfo groupID
        members <- runDB conn $ Sessions.getMembersOfGroup groupID
        return $ Group.Group groupID name mDesc members
getGroupHandler _ _ = throwError errNotLoggedIn

deleteGroupHandler
    :: AuthResult Auth.Token -> Group.GroupID -> Handler NoContent
deleteGroupHandler (Authenticated token) groupID = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID $ do
        runDB conn $ Sessions.deleteGroup groupID
        return NoContent
deleteGroupHandler _ _ = throwError errNotLoggedIn

-- | Updates a group's name and/or description via PATCH
--   Requires SuperAdmin or Admin of the specific group
patchGroupHandler
    :: AuthResult Auth.Token
    -> Group.GroupID
    -> Group.GroupPatch
    -> Handler Group.GroupOverview
patchGroupHandler (Authenticated token) groupID (Group.GroupPatch {..}) = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID $ do
        -- Validate that at least one field is provided
        when (isNothing patchName && isNothing patchDescription) $
            throwError $
                err400
                    { errBody =
                        "\"At least one of 'patchName' or 'patchDescription' must be provided.\""
                    }

        -- If name is being changed, check for uniqueness
        forM_ patchName $ \newName -> do
            exists <- runDB conn $ Sessions.checkGroupNameExistence newName
            when exists $ do
                -- Check if it's the same group (allow keeping same name)
                Group.GroupOverview _ currentName _ <-
                    runDB conn $ Sessions.getGroupInfo groupID
                when (currentName /= newName) $
                    throwError $
                        err409 {errBody = "\"A group with that name exists already.\""}
            runDB conn $ Sessions.updateGroupName groupID newName

        -- Update description if provided
        forM_ patchDescription $ \newDesc ->
            runDB conn $ Sessions.updateGroupDescription groupID newDesc

        -- Return updated group info
        runDB conn $ Sessions.getGroupInfo groupID
patchGroupHandler _ _ _ = throwError errNotLoggedIn

runDB :: Connection -> Session.Session a -> Handler a
runDB conn session = do
    result <- liftIO $ Session.run session conn
    case result of
        Left _ -> throwError errDatabaseAccessFailed
        Right val -> return val

guardSuperAdmin :: Bool -> Handler ()
guardSuperAdmin True = return ()
guardSuperAdmin False = throwError errSuperAdminOnly
