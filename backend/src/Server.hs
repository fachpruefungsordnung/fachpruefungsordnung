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
import Data.UUID (toString)
import Database (getConnection)
import GHC.Int (Int32)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server
import Servant.OpenApi (HasOpenApi (toOpenApi))
import qualified Server.Auth as Auth
import Server.HTTPHeaders (PDF, PDFByteString (..))
import qualified UserManagement.Document as Document
import qualified UserManagement.Group as Group
import qualified UserManagement.User as User
import qualified VersionControl as VC
import Server.Handlers.UserHandlers
import Server.Handlers.GroupHandlers
import Server.Handlers.RoleHandlers
import Server.Handlers.DocumentHandlers
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
        :<|> "logout"
            :> Get
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
        :<|> Auth AuthMethod Auth.Token
            :> "users"
            :> Capture "userId" User.UserID
            :> Get '[JSON] User.FullUser
        :<|> Auth AuthMethod Auth.Token
            :> "users"
            :> Capture "userId" User.UserID
            :> Delete '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "users"
            :> Capture "userId" User.UserID
            :> ReqBody '[JSON] Auth.UserUpdate
            :> Patch '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
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
            :> "roles"
            :> Capture "groupID" Group.GroupID
            :> Capture "userId" User.UserID
            :> Get '[JSON] User.Role
        :<|> Auth AuthMethod Auth.Token
            :> "roles"
            :> Capture "groupID" Group.GroupID
            :> Capture "userId" User.UserID
            :> ReqBody '[JSON] User.Role
            :> Put '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "roles"
            :> Capture "groupID" Group.GroupID
            :> Capture "userId" User.UserID
            :> Delete '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "roles"
            :> "superadmin"
            :> Capture "userId" User.UserID
            :> Post '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "roles"
            :> "superadmin"
            :> Capture "userId" User.UserID
            :> Delete '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> Get '[JSON] ExistingCommit
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> Delete '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> "external"
            :> Get '[JSON] [(User.UserID, Document.DocPermission)]
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> "external"
            :> Capture "userID" User.UserID
            :> Get '[JSON] (Maybe Document.DocPermission)
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> "external"
            :> Capture "userID" User.UserID
            :> ReqBody '[JSON] Document.DocPermission
            :> Post '[JSON] NoContent
        :<|> Auth AuthMethod Auth.Token
            :> "documents"
            :> Capture "documentID" Document.DocumentID
            :> "external"
            :> Capture "userID" User.UserID
            :> Delete '[JSON] NoContent

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
                :<|> logoutHandler cookieSett
             )
        :<|> ( protectedHandler
                :<|> registerHandler
                :<|> getUserHandler
                :<|> deleteUserHandler
                :<|> patchUserHandler
                :<|> createGroupHandler
                :<|> groupMembersHandler
                :<|> deleteGroupHandler
                :<|> getRoleHandler
                :<|> postRoleHandler
                :<|> deleteRoleHandler
                :<|> postSuperadminHandler
                :<|> deleteSuperadminHandler
                :<|> getDocumentHandler
                :<|> deleteDocumentHandler
                :<|> getAllExternalUsersDocumentHandler
                :<|> getExternalUserDocumentHandler
                :<|> postExternalUserDocumentHandler
                :<|> deleteExternalUserDocumentHandler
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
