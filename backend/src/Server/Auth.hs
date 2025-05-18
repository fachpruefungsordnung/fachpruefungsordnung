{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Server.Auth (Token (..)
                  , UserLoginData (..)
                  , UserRegisterData (..)
                  , UserUpdate (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import GHC.Int ( Int32 )
import Servant.Auth.Server (FromJWT, ToJWT)

data Token = Token
    { subject :: UUID
    , isSuperadmin :: Bool
    }
    deriving (Generic, ToJSON, ToJWT, FromJSON, FromJWT)

data UserLoginData = UserLoginData
    { loginEmail :: Text
    , loginPassword :: Text
    }
    deriving (Generic, FromJSON, ToSchema)

data UserRegisterData = UserRegisterData
    { registerName :: Text
    , registerEmail :: Text
    , registerPassword :: Text
    , groupID :: Int32
    }
    deriving (Generic, FromJSON, ToSchema)

data UserUpdate = UserUpdate
  { newName :: Maybe Text
  , newEmail :: Maybe Text
  , newPassword :: Maybe Text
  , newRole :: Maybe (Int32, Text)
  } deriving (Generic, ToJSON, FromJSON, ToSchema)