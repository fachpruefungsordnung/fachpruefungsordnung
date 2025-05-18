{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module UserManagement.User
    ( User (..)
    , FullUser (..)
    , UserInfo (..)
    , Role (..)
    , roleToText
    , textToRole
    )
where

import Data.Aeson
import Data.OpenApi (ToSchema)
import Data.Text
import Data.UUID (UUID)
import GHC.Generics

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

-- | used for necessary user info inside a group
data UserInfo = UserInfo
    { userInfoID :: UUID
    , userInfoName :: Text
    , userInfoEmail :: Text
    , userInfoRole :: Role
    }

data Role = Member | Admin
    deriving (Eq, Generic)

instance Show Role where
    show = \case
        Member -> "user"
        Admin -> "admin"

instance Read Role where
    readsPrec _ s = case lex s of
        [("member", rs)] -> [(Member, rs)]
        [("admin", rs)]  -> [(Admin, rs)]
        _                -> [] 


-- Convert to/from Text
roleToText :: Role -> Text
roleToText = \case
    Member -> "user"
    Admin -> "admin"

textToRole :: Text -> Maybe Role
textToRole = \case
    "user" -> Just Member
    "admin" -> Just Admin
    _ -> Nothing