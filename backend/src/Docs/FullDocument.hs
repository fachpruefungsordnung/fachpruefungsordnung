{-# LANGUAGE DeriveGeneric #-}

module Docs.FullDocument (FullDocument (..)) where

import Docs.Document (Document)
import Docs.TreeRevision (TreeRevision)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)

data FullDocument a = FullDocument
    { header :: Document
    , body :: Maybe (TreeRevision a)
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (FullDocument a)

instance (FromJSON a) => FromJSON (FullDocument a)

instance (ToSchema a) => ToSchema (FullDocument a)
