{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Lsd.AST.Common
    ( KindName (..)
    , TypeName (..)
    , FullTypeName
    , DisplayTypeName (..)
    , Keyword (..)
    , NavTocHeading (..)
    , Fallback (..)
    )
where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Data (Proxy (Proxy))
import Data.OpenApi (ToSchema (declareNamedSchema))
import Data.String (IsString)
import Data.Text (Text)

newtype KindName = KindName String
    deriving (Show, IsString, Eq, Ord)

instance ToJSON KindName where
    toJSON (KindName kind) = toJSON kind

instance FromJSON KindName where
    parseJSON = fmap KindName . parseJSON

instance ToSchema KindName where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)

newtype TypeName = TypeName String
    deriving (Show, IsString, Eq, Ord)

instance ToJSON TypeName where
    toJSON (TypeName type_) = toJSON type_

instance FromJSON TypeName where
    parseJSON = fmap TypeName . parseJSON

instance ToSchema TypeName where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)

type FullTypeName = (KindName, TypeName)

newtype DisplayTypeName = DisplayTypeName String
    deriving (Show, IsString)

instance ToJSON DisplayTypeName where
    toJSON (DisplayTypeName name) = toJSON name

instance FromJSON DisplayTypeName where
    parseJSON = fmap DisplayTypeName . parseJSON

instance ToSchema DisplayTypeName where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)

newtype Keyword = Keyword Text

-- | Heading for the navigation TOC in the frontend, if static (determined by
--   type only).
newtype NavTocHeading = NavTocHeading Text
    deriving (Show)

-- | A wrapper to denote values that should only be used as fallback;
--   typically in case parsing fails.
newtype Fallback a = Fallback a
    deriving (Show)
