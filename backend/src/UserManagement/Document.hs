{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}

module UserManagement.Document
    ( DocumentID
    , DocPermission (..)
    , Permission (..)
    , hasPermission
    , permissionToText
    , textToPermission
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import GHC.Int (Int32)
import Text.Read (readMaybe)

type DocumentID = Int32

data DocPermission = Reader | Reviewer | Editer
    deriving (Eq, Generic)

data Permission = Read | Comment | Edit
    deriving (Eq, Enum, Bounded)

hasPermission :: DocPermission -> Permission -> Bool
hasPermission Reader Read = True
hasPermission Reviewer p = elem p [Read, Comment]
hasPermission Editer p = elem p [Read, Comment, Edit]
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
        Editer -> "editer"

instance Read DocPermission where
    readsPrec _ s = case lex s of
        [("reader", rs)] -> [(Reader, rs)]
        [("reviewer", rs)] -> [(Reviewer, rs)]
        [("editer", rs)] -> [(Editer, rs)]
        _ -> []

-- Convert to/from Text
permissionToText :: DocPermission -> Text
permissionToText = pack . show

textToPermission :: Text -> Maybe DocPermission
textToPermission = readMaybe . unpack
