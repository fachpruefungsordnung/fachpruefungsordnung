{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      : Docs.UserRef
-- Description : Reference to a User
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
--
-- This module defines a reference to a user.
module Docs.UserRef (UserRef (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Text (Text)

import GHC.Generics (Generic)

import UserManagement.User (UserID)

-- A reference to a user.
data UserRef = UserRef
    { identifier :: UserID
    -- ^ the users id
    , name :: Text
    -- ^ the name of the user
    }
    deriving (Eq, Show, Generic)

instance ToJSON UserRef

instance FromJSON UserRef

instance ToSchema UserRef
