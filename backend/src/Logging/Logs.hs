{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Logging.Logs (Source (..), Severity (..), LogMessage (..), Scope) where

import qualified Data.Aeson as Aeson
import Data.Time (UTCTime)
import Data.UUID (UUID)

import Docs.UserRef (UserRef)

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( HasAdditionalProperties (additionalProperties)
    , HasProperties (..)
    , NamedSchema (NamedSchema)
    , OpenApiType (OpenApiObject)
    , Referenced (Inline)
    , ToSchema
    , declareSchemaRef
    )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Lens (HasType (..))
import Data.OpenApi.Schema (ToSchema (..))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import GHC.Generics (Generic)
import Logging.Scope (Scope)

data Severity = Info | Warning | Error deriving (Generic)

instance ToJSON Severity

instance FromJSON Severity

instance ToSchema Severity

data Source = User UserRef | System deriving (Generic)

instance ToJSON Source

instance FromJSON Source

instance ToSchema Source

data LogMessage = LogMessage
    { identifier :: UUID
    , severity :: Severity
    , timestamp :: UTCTime
    , source :: Source
    , scope :: Scope
    , content :: Aeson.Value
    }
    deriving (Generic)

instance ToJSON LogMessage

instance FromJSON LogMessage

instance ToSchema LogMessage where
    declareNamedSchema _ = do
        idSchema <- declareSchemaRef (Proxy :: Proxy UUID)
        severitySchmema <- declareSchemaRef (Proxy :: Proxy Severity)
        timestampSchema <- declareSchemaRef (Proxy :: Proxy UTCTime)
        sourceSchema <- declareSchemaRef (Proxy :: Proxy Source)
        scopeSchema <- declareSchemaRef (Proxy :: Proxy Text)
        let contentSchema =
                mempty
                    & type_ ?~ OpenApiObject
                    & additionalProperties ?~ OpenApi.AdditionalPropertiesAllowed True
        return $
            NamedSchema (Just "LogMessage") $
                mempty
                    & type_
                        ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("identifier", idSchema)
                            , ("severity", severitySchmema)
                            , ("timestamp", timestampSchema)
                            , ("source", sourceSchema)
                            , ("scope", scopeSchema)
                            , ("content", Inline contentSchema)
                            ]
