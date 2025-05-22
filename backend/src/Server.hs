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
import Data.UUID (UUID, toString)
import Data.Vector (toList)
import Database (getConnection)
import GHC.Int (Int32)
import Hasql.Connection (Connection)
import qualified Hasql.Session as Session
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
import Servant.OpenApi (HasOpenApi (toOpenApi))
import qualified Server.Auth as Auth
import Server.HTTPHeaders (PDF, PDFByteString (..))
import Server.HandlerUtil
import UserManagement.Group (Group (..))
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

{- | Cookie means that Auth is implemented via two Cookies.
  One HTTP-only JWT Cookie, which is managed by the browser
  and a XSRF Cookie, which has to be mirrored in a "X-XSRF-TOKEN" Header
-}
type AuthMethod = '[Cookie]

type ProtectedAPI =
    Auth AuthMethod Auth.Token
        :> "protected"
        :> Get '[JSON] String
        :<|> Auth AuthMethod Auth.Token
            :> "register"
            :> ReqBody '[JSON] Auth.UserRegisterData
            :> Post '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "user"
            :> Capture "userId" UUID
            :> Get '[JSON] User.FullUser
        :<|> Auth AuthMethod Auth.Token
            :> "user"
            :> Capture "userId" UUID
            :> Delete '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "user"
            :> ReqBody '[JSON] Auth.UserUpdate
            :> Patch '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "group"
            :> Capture "groupID" Int32
            :> Get '[JSON] [User.UserInfo]
        :<|> Auth AuthMethod Auth.Token
            :> "group"
            :> "create"
            :> ReqBody '[JSON] Group
            :> Post '[JSON] Int32

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
    eConn <- liftIO getConnection
    case eConn of
        Left _ -> throwError $ err401 {errBody = "login failed! Please try again!\n"}
        Right conn -> do
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
                _ -> throwError $ err401 {errBody = "login failed! Please try again!\n"}

registerHandler
    :: AuthResult Auth.Token -> Auth.UserRegisterData -> Handler NoContent
registerHandler (Authenticated Auth.Token {..}) (Auth.UserRegisterData {..}) = do
    eConn <- liftIO getConnection
    case eConn of
        Left _ -> throwError $ err401 {errBody = "connection to db failed\n"}
        Right conn -> do
            eRole <-
                liftIO $ Session.run (Sessions.getUserRoleInGroup subject groupID) conn
            case eRole of
                Right (Just role) ->
                    if role /= User.Admin
                        then
                            throwError $
                                err401
                                    { errBody =
                                        "unauthorized! you must be admin of the specified group to perform this action!\n"
                                    }
                        else do
                            eUser <- liftIO $ Session.run (Sessions.getUser registerEmail) conn
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
                                        Left _ -> throwError $ err401 {errBody = "user creation failed!\n"}
                                        Right userID -> do
                                            eAction' <-
                                                liftIO $ Session.run (Sessions.addRole userID groupID User.Member) conn
                                            case eAction' of
                                                Left _ -> throwError $ err401 {errBody = "failed to assign role!\n"} -- need to delete user here!
                                                Right _ -> return NoContent
                                _ -> throwError $ err401 {errBody = "registration failed! Please try again!\n"}
                _ -> throwError $ err401 {errBody = "registration failed! Please try again!\n"}
registerHandler _ _ =
    throwError
        err403
            { errBody = "Not allowed! You need to login to perform this action.\n"
            }

getUserHandler
    :: AuthResult Auth.Token -> UUID -> Handler User.FullUser
getUserHandler (Authenticated Auth.Token {..}) requestedUserID = do
    eConn <- liftIO getConnection
    case eConn of
        Left _ -> throwError $ err401 {errBody = "connection to db failed\n"}
        Right conn -> do undefined
getUserHandler _ _ =
    throwError
        err403
            { errBody = "Not allowed! You need to login to perform this action.\n"
            }

deleteUserHandler
    :: AuthResult Auth.Token -> UUID -> Handler NoContent
deleteUserHandler (Authenticated Auth.Token {..}) requestedUserID =
    if isSuperadmin
        then do
            eConn <- liftIO getConnection
            case eConn of
                Left _ -> throwError $ err401 {errBody = "connection to db failed\n"}
                Right conn -> do undefined
        else
            throwError $
                err401
                    { errBody = "permission denied! must be superadmin to perform this action.\n"
                    }
deleteUserHandler _ _ =
    throwError
        err403
            { errBody = "Not allowed! You need to login to perform this action.\n"
            }

patchUserHandler
    :: AuthResult Auth.Token -> Auth.UserUpdate -> Handler NoContent
patchUserHandler (Authenticated Auth.Token {..}) (Auth.UserUpdate {..}) = do
    eConn <- liftIO getConnection
    case eConn of
        Left _ -> throwError $ err401 {errBody = "connection to db failed\n"}
        Right conn -> do undefined
patchUserHandler _ _ =
    throwError
        err403
            { errBody = "Not allowed! You need to login to perform this action.\n"
            }

groupMembersHandler :: AuthResult Auth.Token -> Int32 -> Handler [User.UserInfo]
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

createGroupHandler :: AuthResult Auth.Token -> Group -> Handler Int32
createGroupHandler (Authenticated Auth.Token {..}) (Group {..}) = do
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
                        then createGroup conn
                        else
                            throwError $
                                err403 {errBody = "You need to be Admin of any group to perform this action!\n"}
  where
    createGroup :: Connection -> Handler Int32
    createGroup conn = do
        eGroupID <-
            liftIO $ Session.run (Sessions.addGroup groupName groupDescription) conn
        case eGroupID of
            Left _ -> throwError errDatabaseAccessFailed
            Right groupID -> return groupID
createGroupHandler _ _ = throwError errNotLoggedIn

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
                :<|> getUserHandler
                :<|> deleteUserHandler
                :<|> patchUserHandler
                :<|> groupMembersHandler
                :<|> createGroupHandler
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
