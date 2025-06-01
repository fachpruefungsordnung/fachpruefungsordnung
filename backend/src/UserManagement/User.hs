{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module UserManagement.User
    ( User (..)
    , FullUser (..)
    , UserInfo (..)
    , Role (..)
    , UserID
    , roleToText
    , textToRole
    )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text, pack, unpack)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import qualified UserManagement.Group as Group

type UserID = UUID

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
    { fullUserID :: UserID
    , fullUserName :: Text
    , fullUserEmail :: Text
    , fullUserIsSuperadmin :: Bool
    , fullUserRoles :: [(Group.GroupID, Role)]
    }
    deriving (Show, Generic)

instance ToJSON FullUser
instance FromJSON FullUser
instance ToSchema FullUser

-- | used for necessary user info inside a group
data UserInfo = UserInfo
    { userInfoID :: UserID
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
