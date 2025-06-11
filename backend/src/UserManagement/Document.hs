{-# LANGUAGE DeriveGeneric #-}

module UserManagement.Document (DocumentID, DocPermission (..), permissionToText, textToPermission) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import GHC.Int (Int32)
import Text.Read (readMaybe)

type DocumentID = Int32

data DocPermission = Read | Review | Edit
    deriving (Eq, Generic)

instance Ord DocPermission where
    a <= b = permissionRank a <= permissionRank b
      where
        permissionRank :: DocPermission -> Int
        permissionRank Read = 0
        permissionRank Review = 1
        permissionRank Edit = 2

instance ToJSON DocPermission
instance FromJSON DocPermission
instance ToSchema DocPermission

instance Show DocPermission where
    show s = case s of
        Read -> "read"
        Review -> "review"
        Edit -> "edit"

instance Read DocPermission where
    readsPrec _ s = case lex s of
        [("read", rs)] -> [(Read, rs)]
        [("review", rs)] -> [(Review, rs)]
        [("edit", rs)] -> [(Edit, rs)]
        _ -> []

-- Convert to/from Text
permissionToText :: DocPermission -> Text
permissionToText = pack . show

textToPermission :: Text -> Maybe DocPermission
textToPermission = readMaybe . unpack
