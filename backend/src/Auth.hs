{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Auth (Token (..), UserLoginData (..), UserRegisterData (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
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
    , groupName :: Text
    }
    deriving (Generic, FromJSON, ToSchema)
