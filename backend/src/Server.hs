{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server (runServer, DocumentedAPI, PublicAPI, jwtSettings, cookieSettings, app, server) where

import qualified Auth
import Control.Lens
import Control.Monad.IO.Class
import Crypto.JOSE.JWK (JWK)
import Data.ByteString.Lazy (ByteString, readFile)
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
import Data.UUID ( toString )
import Data.Vector
import Database (getConnection)
import GHC.Int
import HTTPHeaders (PDF)
import qualified Hasql.Session as Session
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Auth.Server
import Servant.OpenApi
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

type ProtectedAPI auths =
         Auth auths Auth.Token :> "protected" :> Get '[JSON] String
        -- in protected for now because their is no ToSchema for ByteString
    :<|> "document" :> Get '[PDF] ByteString
    :<|> Auth auths Auth.Token :> "register"
                                :> ReqBody '[JSON] Auth.UserRegisterData
                                :> Post '[JSON] NoContent

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type DocumentedAPI auths = SwaggerAPI :<|> PublicAPI :<|> ProtectedAPI auths

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

documentHandler :: Handler ByteString
documentHandler = liftIO $ readFile "static/dummy.pdf"

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

registerHandler :: AuthResult Auth.Token -> Auth.UserRegisterData -> Handler NoContent
registerHandler (Authenticated Auth.Token{..}) (Auth.UserRegisterData{..}) = do
    eConn <- liftIO getConnection
    case eConn of
        Left _ -> throwError $ err401 {errBody = "registration failed! Please try again!\n"}
        Right conn -> do
            eRole <- liftIO $ Session.run (Sessions.getUserRoleInGroup subject groupName) conn
            case eRole of 
                Right (Just role) -> 
                    if role /= "admin" 
                    then throwError $ err401 {errBody = "unauthorized! you must be admin of the specified group to perform this action!\n"}
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
                                    Left _ -> throwError $ err401 {errBody = "registration failed! Please try again!\n"}
                                    Right _ -> do
                                        return NoContent
                            _ -> throwError $ err401 {errBody = "registration failed! Please try again!\n"}
                _ -> throwError $ err401 {errBody = "registration failed! Please try again!\n"}
registerHandler _ _ =
    throwError
        err403
            { errBody = "Not allowed! You need to login to see this content.\n"
            }

api :: Proxy PublicAPI
api = Proxy

swagger :: OpenApi
swagger =
    toOpenApi api
        & info . title .~ "Fachprüfungsordnung API"
        & info . version .~ "1.0"
        & info . description ?~ "This is the API for the Fachprüfungsordnung editor."
        & info . license ?~ "AGPL3"
        & servers .~ ["https://batailley.informatik.uni-kiel.de/api/"]

server :: CookieSettings -> JWTSettings -> Server (DocumentedAPI auths)
server cookieSett jwtSett =
    return swagger
        :<|> ( pingHandler
                :<|> userHandler
                :<|> debugAPIHandler
                :<|> loginHandler cookieSett jwtSett
                
             )
        :<|> ( protectedHandler
                :<|> documentHandler
                :<|> registerHandler
             )

documentedAPI :: Proxy (DocumentedAPI '[JWT, Cookie])
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
