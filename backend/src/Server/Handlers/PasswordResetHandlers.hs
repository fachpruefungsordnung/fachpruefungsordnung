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

import Control.Monad.IO.Class (liftIO)
import Data.Password.Argon2
    ( PasswordHash (unPasswordHash)
    , hashPassword
    , mkPassword
    )
import Data.Time (getCurrentTime)
import qualified Hasql.Session as Session
import Servant
import Server.Auth.PasswordReset
import Server.Auth.PasswordResetUtil (getTokenExpirationTime)
import qualified Server.Auth.PasswordResetUtil as Util
import Server.HandlerUtil
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
                    -- Create reset URL (you might want to make this configurable)
                    let resetUrl = Util.createResetUrl "https://batailley.informatik.uni-kiel.de" token

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

-- | Handler for password reset confirmation
confirmPasswordResetHandler :: PasswordResetConfirm -> Handler NoContent
confirmPasswordResetHandler PasswordResetConfirm {..} = do
    -- Validate token format
    if not (Util.validateTokenFormat resetConfirmToken)
        then throwError $ err400 {errBody = "Invalid token format"}
        else do
            conn <- tryGetDBConnection
            let tokenHash = Util.hashToken resetConfirmToken

            -- Look up token in database
            eToken <- liftIO $ Session.run (Sessions.getPasswordResetToken tokenHash) conn
            case eToken of
                Right (Just (_, userId, _, expiresAt, _, usedAt)) -> do
                    -- Check if token is already used
                    case usedAt of
                        Just _ -> throwError $ err400 {errBody = "Token has already been used"}
                        Nothing -> do
                            -- Check if token is expired (redundant with DB query, but good for clarity)
                            now <- liftIO getCurrentTime
                            if now > expiresAt
                                then throwError $ err400 {errBody = "Token has expired"}
                                else do
                                    -- Hash new password
                                    hashedPassword <- liftIO $ hashPassword (mkPassword resetConfirmNewPassword)

                                    -- Update user's password
                                    eUpdateResult <-
                                        liftIO $
                                            Session.run
                                                (Sessions.updateUserPWHash userId (unPasswordHash hashedPassword))
                                                conn

                                    case eUpdateResult of
                                        Right _ -> do
                                            -- Mark token as used
                                            _ <-
                                                liftIO $
                                                    Session.run
                                                        (Sessions.markPasswordResetTokenUsed tokenHash)
                                                        conn

                                            -- Clean up expired tokens (best effort, don't fail if it doesn't work)
                                            _ <- liftIO $ Session.run Sessions.cleanupExpiredTokens conn

                                            return NoContent
                                        Left _ -> throwError errDatabaseAccessFailed
                Right Nothing ->
                    throwError $ err400 {errBody = "Invalid or expired token"}
                Left _ -> throwError errDatabaseAccessFailed
