{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}

module UserManagement.DocumentPermission
    ( DocPermission (..)
    , Permission (..)
    , hasPermission
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

data DocPermission = Reader | Reviewer | Editor
    deriving (Eq, Generic)

data Permission = Read | Comment | Edit
    deriving (Eq, Enum, Bounded)

hasPermission :: DocPermission -> Permission -> Bool
hasPermission Reader Read = True
hasPermission Reviewer p = elem p [Read, Comment]
hasPermission Editor p = elem p [Read, Comment, Edit]
hasPermission _ _ = False

instance Ord DocPermission where
    (<=) :: DocPermission -> DocPermission -> Bool
    a <= b =
        all
            (\p -> not (hasPermission a p) || hasPermission b p)
            [minBound .. maxBound] -- list of all Permissions

instance ToJSON DocPermission
instance FromJSON DocPermission
instance ToSchema DocPermission

instance Show DocPermission where
    show s = case s of
        Reader -> "reader"
        Reviewer -> "reviewer"
        Editor -> "editor"

instance Read DocPermission where
    readsPrec _ s = case lex s of
        [("reader", rs)] -> [(Reader, rs)]
        [("reviewer", rs)] -> [(Reviewer, rs)]
        [("editor", rs)] -> [(Editor, rs)]
        _ -> []

-- Convert to/from Text
permissionToText :: DocPermission -> Text
permissionToText = pack . show

textToPermission :: Text -> Maybe DocPermission
textToPermission = readMaybe . unpack

data UsersPermission = UsersPermission
    { userID :: User.UserID
    , permission :: DocPermission
    }
    deriving (Generic)

instance ToJSON UsersPermission
instance FromJSON UsersPermission
instance ToSchema UsersPermission

data DocumentWithPermission = DocumentWithPermission
    { documentPermission :: DocPermission
    , document :: Document.Document
    }
    deriving (Generic)

instance ToJSON DocumentWithPermission
instance ToSchema DocumentWithPermission
