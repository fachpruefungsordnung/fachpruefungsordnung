{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Docs.FullDocument
-- Description : Document With Emplaced Tree and Text Revisions
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
module Docs.FullDocument (FullDocument (..)) where

import Docs.Document (Document)
import Docs.TreeRevision (TreeRevision)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import GHC.Generics (Generic)

-- | A »full« document, with a @TreeRevision@, where @TextElementRevision@s are
-- emplaced for all @TextElement@s.
data FullDocument a = FullDocument
    { header :: Document
    -- ^ metadata about the @Document@
    , body :: Maybe (TreeRevision a)
    -- ^ a »full« @TreeRevision@ for the @Document@
    }
    deriving (Generic)

instance (ToJSON a) => ToJSON (FullDocument a)

instance (FromJSON a) => FromJSON (FullDocument a)

instance (ToSchema a) => ToSchema (FullDocument a)
