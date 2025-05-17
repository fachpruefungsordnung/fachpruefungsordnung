{-# LANGUAGE DeriveGeneric #-}

module UserManagement.User
    ( User (..),
      FullUser(..)
    )
where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Text
import GHC.Generics
import Data.UUID (UUID)

data User = User
    { userName :: Text
    , userEmail :: Text
    , userPwhash :: Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON User

instance FromJSON User

instance ToSchema User

data FullUser = FullUser
    { fullUserID :: UUID
    , fullUserName :: Text
    , fullUserEmail :: Text
    , fullUserPwhash :: Text
    , fullUserRoles :: [(Text, Text)]
    }
    deriving (Eq, Show, Generic)

instance ToJSON FullUser

instance FromJSON FullUser

instance ToSchema FullUser
