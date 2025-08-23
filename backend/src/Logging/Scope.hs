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

import Data.Aeson (FromJSON (parseJSON), ToJSON)
import Data.Aeson.Types (ToJSON (toJSON))
import Data.OpenApi (ToSchema)
import Data.OpenApi.Schema (ToSchema (declareNamedSchema))
import Data.Proxy (Proxy (Proxy))

newtype Scope = Scope
    { unScope :: Text
    }

instance ToJSON Scope where
    toJSON = toJSON . unScope

instance FromJSON Scope where
    parseJSON = fmap Scope . parseJSON

instance ToSchema Scope where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)

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
