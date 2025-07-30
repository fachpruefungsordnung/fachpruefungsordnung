{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}

module UserManagement.DocumentPermission
    ( Permission (..)
    , permissionToText
    , textToPermission
    , UsersPermission (..)
    , DocumentWithPermission (..)
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text, pack, unpack)
import DocumentManagement.Document as Document
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import UserManagement.User as User (UserID)

data Permission = Read | Comment | Edit
    deriving (Generic, Eq, Enum, Ord, Bounded)

instance Show Permission where
    show s = case s of
        Read -> "read"
        Comment -> "comment"
        Edit -> "edit"

instance Read Permission where
    readsPrec _ s = case lex s of
        [("read", rs)] -> [(Read, rs)]
        [("comment", rs)] -> [(Comment, rs)]
        [("edit", rs)] -> [(Edit, rs)]
        _ -> []

instance ToJSON Permission
instance FromJSON Permission
instance ToSchema Permission

-- Convert to/from Text
permissionToText :: Permission -> Text
permissionToText = pack . show

textToPermission :: Text -> Maybe Permission
textToPermission = readMaybe . unpack

data UsersPermission = UsersPermission
    { userID :: User.UserID
    , permission :: Permission
    }
    deriving (Generic)

instance ToJSON UsersPermission
instance FromJSON UsersPermission
instance ToSchema UsersPermission

data DocumentWithPermission = DocumentWithPermission
    { documentPermission :: Permission
    , document :: Document.Document
    }
    deriving (Generic)

instance ToJSON DocumentWithPermission
instance ToSchema DocumentWithPermission
