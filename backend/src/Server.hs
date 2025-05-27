{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (runServer, DocumentedAPI, PublicAPI, jwtSettings, cookieSettings, app, server) where

import Control.Lens
import Control.Monad.IO.Class
import Crypto.JOSE.JWK (JWK)
import Data.ByteString.Lazy (readFile)
import Data.OpenApi
    ( OpenApi
    , description
    , info
    , license
    , servers
    , title
    , version
    )
import Data.Password.Argon2
import Data.Text (Text)
import Data.UUID (toString)
import Data.Vector (toList)
import Database (getConnection)
import GHC.Int (Int32)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
import Servant.OpenApi (HasOpenApi (toOpenApi))
import qualified Server.Auth as Auth
import Server.HTTPHeaders (PDF, PDFByteString (..))
import Server.HandlerUtil
import qualified UserManagement.Group as Group
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User
import qualified VersionControl as VC
import VersionControl.Commit
import Prelude hiding (readFile)

type DebugAPI =
    "commits" :> Capture "id" Int32 :> Get '[JSON] ExistingCommit
        :<|> "commits" :> ReqBody '[JSON] CreateCommit :> Post '[JSON] ExistingCommit

type PublicAPI =
    "ping" :> Get '[JSON] String
        :<|> "users" :> Get '[JSON] [User.User]
        :<|> "document" :> Get '[PDF] PDFByteString
        :<|> DebugAPI
        :<|> "login"
            :> ReqBody '[JSON] Auth.UserLoginData
            :> Post
                '[JSON]
                ( Headers
                    '[ Header "Set-Cookie" SetCookie
                     , Header "Set-Cookie" SetCookie
                     ]
                    NoContent
                )

-- | Cookie means that Auth is implemented via two Cookies.
--   One HTTP-only JWT Cookie, which is managed by the browser
--   and a XSRF Cookie, which has to be mirrored in a "X-XSRF-TOKEN" Header
type AuthMethod = '[Cookie]

type ProtectedAPI =
    Auth AuthMethod Auth.Token
        :> "protected"
        :> Get '[JSON] String
        :<|> Auth AuthMethod Auth.Token
            :> "register"
            :> ReqBody '[JSON] Auth.UserRegisterData
            :> Post '[JSON] NoContent
        -- :<|> Auth AuthMethod Auth.Token
        --     :> "users"
        --     :> Capture "userId" User.UserID
        --     :> Get '[JSON] User.FullUser
        :<|> Auth AuthMethod Auth.Token
            :> "users"
<<<<<<< HEAD
=======
            :> Capture "userId" User.UserID
            :> Get '[JSON] User.FullUser
        :<|> Auth AuthMethod Auth.Token
            :> "user"
>>>>>>> 01650f5 (added role endpoints and logic)
            :> Capture "userId" User.UserID
            :> Delete '[JSON] NoContent
        -- :<|> Auth AuthMethod Auth.Token
        --     :> "users"
        --     :> ReqBody '[JSON] Auth.UserUpdate
        --     :> Patch '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
<<<<<<< HEAD
            :> "groups"
=======
            :> "users"
            :> Capture "userId" User.UserID
            :> ReqBody '[JSON] Auth.UserUpdate
            :> Patch '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "group"
            :> "create"
>>>>>>> 01650f5 (added role endpoints and logic)
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
<<<<<<< HEAD
=======
        :<|> Auth AuthMethod Auth.Token
            :> "roles"
            :> Capture "groupID" Group.GroupID
            :> Capture "userId" User.UserID
            :> Get '[JSON] User.Role
        :<|> Auth AuthMethod Auth.Token
            :> "roles"
            :> Capture "groupID" Group.GroupID
            :> Capture "userId" User.UserID
            :> ReqBody '[JSON] User.Role
            :> Post '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "roles"
            :> Capture "groupID" Group.GroupID
            :> Capture "userId" User.UserID
            :> Delete '[JSON] NoContent
        
>>>>>>> 01650f5 (added role endpoints and logic)

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type DocumentedAPI = SwaggerAPI :<|> PublicAPI :<|> ProtectedAPI

pingHandler :: Handler String
pingHandler = return "pong"

getCommitHandler :: Int32 -> Handler ExistingCommit
getCommitHandler id' = liftIO $ do
    Right connection <- getConnection
    Right commit <- VC.getCommit (CommitID id') $ VC.Context connection
    return commit

postCommitHandler :: CreateCommit -> Handler ExistingCommit
postCommitHandler commit = liftIO $ do
    Right connection <- getConnection
    Right newCommit <- VC.createCommit commit $ VC.Context connection
    return newCommit

debugAPIHandler
    :: (Int32 -> Handler ExistingCommit)
        :<|> (CreateCommit -> Handler ExistingCommit)
debugAPIHandler = getCommitHandler :<|> postCommitHandler

documentHandler :: Handler PDFByteString
documentHandler = liftIO $ do
    bs <- readFile "static/dummy.pdf"
    return $ PDFByteString bs

protectedHandler :: AuthResult Auth.Token -> Handler String
protectedHandler (Authenticated Auth.Token {..}) =
    return $ "This is very private content of " <> toString subject <> "!"
protectedHandler _ =
    throwError
        err403
            { errBody = "Not allowed! You need to login to see this content.\n"
            }

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
                    mLoginAccepted <-
                        liftIO $ acceptLogin cookieSett jwtSett (Auth.Token uid False)
                    case mLoginAccepted of
                        Nothing -> throwError $ err401 {errBody = "login failed! Please try again!\n"}
                        Just addHeaders -> return $ addHeaders NoContent
        Right Nothing -> throwError $ err401 {errBody = "login failed! Please try again!\n"}
        Left _ -> throwError errDatabaseAccessFailed

registerHandler
    :: AuthResult Auth.Token -> Auth.UserRegisterData -> Handler NoContent
registerHandler (Authenticated token) regData@(Auth.UserRegisterData _ _ _ gID) = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token gID (addNewMember regData conn)
  where
    addNewMember :: Auth.UserRegisterData -> Connection -> Handler NoContent
    addNewMember (Auth.UserRegisterData {..}) conn = do
<<<<<<< HEAD
        eUser <- liftIO $ Session.run (Sessions.getUser registerEmail) conn
=======
        eUser <- liftIO $ Session.run (Sessions.getUserByEmail registerEmail) conn
>>>>>>> 01650f5 (added role endpoints and logic)
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

<<<<<<< HEAD
-- getUserHandler
--     :: AuthResult Auth.Token -> User.UserID -> Handler User.FullUser
-- getUserHandler (Authenticated Auth.Token {..}) requestedUserID = do
--     conn <- tryGetDBConnection
--     undefined
-- getUserHandler _ _ = throwError errNotLoggedIn
=======
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
                Right (Just User.User{..}) -> do
                    eAction' <- liftIO $ Session.run (Sessions.getAllUserRoles requestedUserID) conn
                    case eAction' of
                        Left _ -> throwError errDatabaseAccessFailed
                        Right roles ->
                            let roles' = [(group, role) | (group, Just role) <- roles] in
                            return $ User.FullUser requestedUserID userName userEmail roles'
        else
            throwError errSuperAdminOnly
getUserHandler _ _ = throwError errNotLoggedIn
>>>>>>> 01650f5 (added role endpoints and logic)

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

<<<<<<< HEAD
-- patchUserHandler
--     :: AuthResult Auth.Token -> Auth.UserUpdate -> Handler NoContent
-- patchUserHandler (Authenticated Auth.Token {..}) (Auth.UserUpdate {..}) = do
--     conn <- tryGetDBConnection
--     undefined
-- patchUserHandler _ _ = throwError errNotLoggedIn
=======
patchUserHandler
    :: AuthResult Auth.Token -> User.UserID -> Auth.UserUpdate -> Handler NoContent
patchUserHandler (Authenticated Auth.Token{..}) userID (Auth.UserUpdate {..}) = do
    conn <- tryGetDBConnection
    if isSuperadmin || subject == userID then
        patchUser conn
    else
        throwError errSuperAdminOnly
  where
    patchUser :: Connection -> Handler NoContent
    patchUser conn = do
        updateEntry conn newName $ Sessions.updateUserName userID
        updateEntry conn newEmail $ Sessions.updateUserEmail userID
        return NoContent

    updateEntry :: Connection -> Maybe a -> (a -> Session.Session ()) -> Handler ()
    updateEntry _    Nothing    _   = return ()
    updateEntry conn (Just val) upd = do
        eAction <- liftIO $ Session.run (upd val) conn
        case eAction of
            Left _ -> throwError errDatabaseAccessFailed
            Right _ -> return ()

patchUserHandler _ _ _ = throwError errNotLoggedIn
>>>>>>> 01650f5 (added role endpoints and logic)

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

<<<<<<< HEAD
=======
getRoleHandler 
    :: AuthResult Auth.Token -> Group.GroupID -> User.UserID -> Handler User.Role
getRoleHandler (Authenticated token) groupID userID = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID (getRole conn)
  where
    getRole :: Connection -> Handler User.Role
    getRole conn = do
        eResult <- liftIO $ Session.run (Sessions.getUserRoleInGroup userID groupID) conn
        case eResult of
            Left _ -> throwError errDatabaseAccessFailed
            Right (Just role) -> return role
            Right Nothing -> throwError $ err404 {errBody = "User not member of this group."}
getRoleHandler _ _ _ = throwError errNotLoggedIn

postRoleHandler 
    :: AuthResult Auth.Token -> Group.GroupID -> User.UserID -> User.Role -> Handler NoContent
postRoleHandler (Authenticated token) groupID userID userRole = do
    conn <- tryGetDBConnection
    ifSuperOrAdminDo conn token groupID (postRole conn)
  where
    postRole :: Connection -> Handler NoContent
    postRole conn = do
        eResult <- liftIO $ Session.run (Sessions.getUserRoleInGroup userID groupID) conn
        case eResult of
            Left _ -> throwError errDatabaseAccessFailed
            Right (Just role) -> if role == userRole 
                                 then return NoContent
                                 else do
                                    eAction <- liftIO $ Session.run (Sessions.updateUserRoleInGroup userID groupID userRole) conn
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
        eResult <- liftIO $ Session.run (Sessions.removeUserFromGroup userID groupID) conn
        case eResult of
            Left _ -> throwError errDatabaseAccessFailed
            Right _ -> return NoContent
deleteRoleHandler _ _ _ = throwError errNotLoggedIn

>>>>>>> 01650f5 (added role endpoints and logic)
api :: Proxy (PublicAPI :<|> ProtectedAPI)
api = Proxy

swagger :: OpenApi
swagger =
    toOpenApi api
        & info . title .~ "Fachprüfungsordnung API"
        & info . version .~ "1.0"
        & info . description ?~ "This is the API for the Fachprüfungsordnung editor."
        & info . license ?~ "AGPL3"
        & servers .~ ["https://batailley.informatik.uni-kiel.de/api/"]

server :: CookieSettings -> JWTSettings -> Server DocumentedAPI
server cookieSett jwtSett =
    return swagger
        :<|> ( pingHandler
                :<|> userHandler
                :<|> documentHandler
                :<|> debugAPIHandler
                :<|> loginHandler cookieSett jwtSett
             )
        :<|> ( protectedHandler
                :<|> registerHandler
                -- :<|> getUserHandler
                :<|> deleteUserHandler
                -- :<|> patchUserHandler
                :<|> createGroupHandler
                :<|> groupMembersHandler
                :<|> deleteGroupHandler
                :<|> getRoleHandler
                :<|> postRoleHandler
                :<|> deleteRoleHandler
             )

documentedAPI :: Proxy DocumentedAPI
documentedAPI = Proxy

app :: CookieSettings -> JWTSettings -> Application
app cookieSett jwtSett =
    serveWithContext
        documentedAPI
        (cookieSett :. jwtSett :. EmptyContext)
        (server cookieSett jwtSett)

jwtSettings :: JWK -> JWTSettings
jwtSettings = defaultJWTSettings

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings

runServer :: IO ()
runServer = do
    let port = 80
    jwtSecretKey <- generateKey
    let jwtSett = jwtSettings jwtSecretKey
    run port (app cookieSettings jwtSett)
