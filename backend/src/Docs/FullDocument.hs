{-# LANGUAGE DeriveGeneric #-}

module Docs.FullDocument (FullDocument (..)) where

import Docs.Document (Document)
import Docs.TextRevision (TextElementRevision)
import Docs.TreeRevision (TreeRevision)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)

data FullDocument = FullDocument
    { header :: Document
    , body :: Maybe (TreeRevision TextElementRevision)
    }
    deriving (Generic)

instance ToJSON FullDocument

instance FromJSON FullDocument

instance ToSchema FullDocument
