{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Logging.Scope
    ( Scope (..)
    , server
    , database
    , logging
    , docs
    , docsText
    , docsTextRevision
    , docsTree
    , docsTreeRevision
    , docsComment
    , userManagement
    ) where

import Data.Text (Text)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)

newtype Scope = Scope
    { unScope :: Text
    }
    deriving (Generic)

instance ToJSON Scope

instance FromJSON Scope

instance ToSchema Scope

server :: Scope
server = Scope "server"

database :: Scope
database = Scope "database"

logging :: Scope
logging = Scope "logging"

docs :: Scope
docs = Scope "docs"

docsText :: Scope
docsText = Scope "docs.text"

docsTextRevision :: Scope
docsTextRevision = Scope "docs.text.revision"

docsTree :: Scope
docsTree = Scope "docs.tree"

docsTreeRevision :: Scope
docsTreeRevision = Scope "docs.tree.revision"

docsComment :: Scope
docsComment = Scope "docs.comment"

userManagement :: Scope
userManagement = Scope "user-management"
