{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Server.Auth.PasswordReset
    ( PasswordResetRequest (..)
    , PasswordResetConfirm (..)
    , PasswordResetToken (..)
    , PasswordResetAPI
    ) where

import Data.Aeson (FromJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Servant.API
import UserManagement.User (UserID)

-- | Request data for initiating a password reset
newtype PasswordResetRequest = PasswordResetRequest
    { resetRequestEmail :: Text
    }
    deriving (Generic, FromJSON, ToSchema)

-- | Data for confirming a password reset with token
data PasswordResetConfirm = PasswordResetConfirm
    { resetConfirmToken :: Text
    , resetConfirmNewPassword :: Text
    }
    deriving (Generic, FromJSON, ToSchema)

-- | Internal representation of a password reset token
data PasswordResetToken = PasswordResetToken
    { tokenId :: UUID
    , tokenUserId :: UserID
    , tokenHash :: Text
    , tokenExpiresAt :: UTCTime
    , tokenCreatedAt :: UTCTime
    , tokenUsedAt :: Maybe UTCTime
    }
    deriving (Generic, Show, Eq)

-- | API definition for password reset endpoints
type PasswordResetAPI =
    "password-reset"
        :> ( "request" :> ReqBody '[JSON] PasswordResetRequest :> Post '[JSON] NoContent
                :<|> "confirm" :> ReqBody '[JSON] PasswordResetConfirm :> Post '[JSON] NoContent
           )
