{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Server.Handlers.PasswordResetHandlers
    ( PasswordResetAPI
    , passwordResetServer
    , requestPasswordResetHandler
    , confirmPasswordResetHandler
    ) where

import Control.Monad (unless, when)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Maybe (isJust)
import Data.Password.Argon2
    ( PasswordHash (unPasswordHash)
    , hashPassword
    , mkPassword
    )
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import qualified Hasql.Session as Session
import Servant
import Server.Auth.PasswordReset
import Server.Auth.PasswordResetUtil (getTokenExpirationTime)
import qualified Server.Auth.PasswordResetUtil as Util
import Server.HandlerUtil
import System.Environment (getEnv)
import qualified UserManagement.Sessions as Sessions
import qualified UserManagement.User as User

-- | Server implementation for password reset API
passwordResetServer :: Server PasswordResetAPI
passwordResetServer = requestPasswordResetHandler :<|> confirmPasswordResetHandler

-- | Handler for password reset requests
requestPasswordResetHandler :: PasswordResetRequest -> Handler NoContent
requestPasswordResetHandler PasswordResetRequest {..} = do
    conn <- tryGetDBConnection

    -- Check if user exists
    eUser <- liftIO $ Session.run (Sessions.getUserByEmail resetRequestEmail) conn
    case eUser of
        Right (Just user) -> do
            -- Generate reset token
            token <- liftIO Util.generateResetToken
            let tokenHash = Util.hashToken token

            expiresAt <- liftIO getTokenExpirationTime

            -- Store token in database
            eTokenId <-
                liftIO $
                    Session.run
                        (Sessions.createPasswordResetToken (User.userID user) tokenHash expiresAt)
                        conn

            case eTokenId of
                Right _ -> do
                    baseUrl <- liftIO $ getEnv "SERVER_HOST" <&> Text.pack
                    let resetUrl = Util.createResetUrl baseUrl token

                    -- Send email
                    liftIO $
                        Util.sendPasswordResetEmail
                            (User.userEmail user)
                            (User.userName user)
                            resetUrl

                    return NoContent
                Left _ -> throwError errDatabaseAccessFailed
        Right Nothing ->
            -- For security, don't reveal whether email exists
            -- Just return success but don't send email
            return NoContent
        Left _ -> throwError errDatabaseAccessFailed

confirmPasswordResetHandler :: PasswordResetConfirm -> Handler NoContent
confirmPasswordResetHandler PasswordResetConfirm {..} =
    do
        unless (Util.validateTokenFormat resetConfirmToken) $
            throwError $
                err400 {errBody = "Invalid token format"}

        conn <- tryGetDBConnection
        let tokenHash = Util.hashToken resetConfirmToken

        runExceptT $
            do
                -- Look up token
                eToken <- liftIO $ Session.run (Sessions.getPasswordResetToken tokenHash) conn
                (_, userId, _, expiresAt, _, usedAt) <- case eToken of
                    Right (Just t) -> pure t
                    Right Nothing -> throwError $ err400 {errBody = "Invalid or expired token"}
                    Left _ -> throwError errDatabaseAccessFailed

                -- Check if already used
                when (isJust usedAt) $
                    throwError $
                        err400 {errBody = "Token has already been used"}

                -- Check expiration
                now <- liftIO getCurrentTime
                when (now > expiresAt) $
                    throwError $
                        err400 {errBody = "Token has expired"}

                -- Hash new password
                hashedPassword <- liftIO $ hashPassword (mkPassword resetConfirmNewPassword)

                -- Update password
                eUpdate <-
                    liftIO $
                        Session.run
                            (Sessions.updateUserPWHash userId (unPasswordHash hashedPassword))
                            conn
                case eUpdate of
                    Right _ -> pure ()
                    Left _ -> throwError errDatabaseAccessFailed

                -- Mark token as used (ignore failures here)
                _ <- liftIO $ Session.run (Sessions.markPasswordResetTokenUsed tokenHash) conn
                -- Cleanup expired tokens (best effort)
                _ <- liftIO $ Session.run Sessions.cleanupExpiredTokens conn

                pure NoContent
        >>= either throwError pure
