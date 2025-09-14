{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.AuthHandlers
    ( AuthAPI
    , authServer
    ) where

import Control.Monad.IO.Class
import Data.Password.Argon2
import qualified Hasql.Session as Session
import Servant
import Servant.Auth.Server
import qualified Server.Auth as Auth
import Server.HandlerUtil
import qualified UserManagement.Sessions as Sessions
import Prelude hiding (readFile)

type AuthAPI =
    "login"
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

authServer :: CookieSettings -> JWTSettings -> Server AuthAPI
authServer cookieSett jwtSett =
    loginHandler cookieSett jwtSett
        :<|> logoutHandler cookieSett

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
                PasswordCheckFail -> throwError errWrongLoginCredentials
                PasswordCheckSuccess -> do
                    eSuperadmin <- liftIO $ Session.run (Sessions.checkSuperadmin uid) conn
                    case eSuperadmin of
                        Left _ -> throwError errDatabaseAccessFailed
                        Right isSuperadmin -> do
                            mLoginAccepted <-
                                liftIO $ acceptLogin cookieSett jwtSett (Auth.Token uid isSuperadmin)
                            case mLoginAccepted of
                                Nothing -> throwError errLoginFailed
                                Just addHeaders -> return $ addHeaders NoContent
        Right Nothing -> throwError errWrongLoginCredentials
        Left _ -> throwError errDatabaseAccessFailed

logoutHandler
    :: CookieSettings
    -> Handler
        ( Headers
            '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
            NoContent
        )
logoutHandler cookieSett = return $ clearSession cookieSett NoContent
