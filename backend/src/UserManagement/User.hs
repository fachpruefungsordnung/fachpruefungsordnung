{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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

import Data.Aeson ( FromJSON, ToJSON )
import Data.OpenApi (ToSchema)
import Data.Text ( pack, unpack, Text )
import Data.UUID (UUID)
import GHC.Generics ( Generic )
import GHC.Int ( Int32 )
import Text.Read (readMaybe)

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
    , fullUserRoles :: [(Int32, Role)]
    }
    deriving (Show, Generic)

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
    deriving (Eq, Show, Generic)

instance ToJSON UserInfo
instance ToSchema UserInfo

data Role = Member | Admin
    deriving (Eq, Generic, FromJSON)

instance ToJSON Role
instance ToSchema Role

instance Show Role where
    show = \case
        Member -> "member"
        Admin -> "admin"

instance Read Role where
    readsPrec _ s = case lex s of
        [("member", rs)] -> [(Member, rs)]
        [("admin", rs)] -> [(Admin, rs)]
        _ -> []

-- Convert to/from Text
roleToText :: Role -> Text
roleToText = pack . show

textToRole :: Text -> Maybe Role
textToRole = readMaybe . unpack
