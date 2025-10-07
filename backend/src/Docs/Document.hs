{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Docs.Document
-- Description : Document and Related Datatypes
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
--
-- This module contains the definition of a @Document@ datatype as well as
-- related datatypes
module Docs.Document
    ( DocumentID (..)
    , Document (..)
    ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time (UTCTime)

import GHC.Generics (Generic)
import GHC.Int (Int64)

import Web.HttpApiData (FromHttpApiData (..))

import Control.Lens ((&), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.OpenApi
    ( OpenApiType (..)
    , ToParamSchema (..)
    , ToSchema (..)
    , exclusiveMinimum
    , minimum_
    , type_
    )

import UserManagement.Group (GroupID)

import Docs.UserRef (UserRef)

-- | ID for a document
newtype DocumentID = DocumentID
    { unDocumentID :: Int64
    }
    deriving (Eq, Show)

instance ToJSON DocumentID where
    toJSON = toJSON . unDocumentID

instance FromJSON DocumentID where
    parseJSON = fmap DocumentID . parseJSON

instance ToSchema DocumentID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToParamSchema DocumentID where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiInteger
            & minimum_ ?~ 0
            & exclusiveMinimum ?~ False

instance FromHttpApiData DocumentID where
    parseUrlPiece = (DocumentID <$>) . parseUrlPiece

-- | Document metadata
data Document = Document
    { identifier :: DocumentID
    , name :: Text
    , group :: GroupID
    , created :: UTCTime
    , createdBy :: UserRef
    , lastEdited :: UTCTime
    , lastEditedBy :: UserRef
    }
    deriving (Eq, Show, Generic)

instance ToJSON Document

instance FromJSON Document

instance ToSchema Document
