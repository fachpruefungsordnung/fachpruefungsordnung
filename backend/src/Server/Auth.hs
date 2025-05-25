{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server.Auth
    ( Token (..)
    , UserLoginData (..)
    , UserRegisterData (..)
    , UserUpdate (..)
    ) where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.HashMap.Strict.InsOrd as HM
import Data.OpenApi
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API
import Servant.Auth.Server
import Servant.OpenApi
import qualified UserManagement.Group as Group
import qualified UserManagement.User as User

data Token = Token
    { subject :: User.UserID
    , isSuperadmin :: Bool
    }
    deriving (Generic, ToJSON, ToJWT, FromJSON, FromJWT)

data UserLoginData = UserLoginData
    { loginEmail :: Text
    , loginPassword :: Text
    }
    deriving (Generic, FromJSON, ToSchema)

data UserRegisterData = UserRegisterData
    { registerName :: Text
    , registerEmail :: Text
    , registerPassword :: Text
    , groupID :: Group.GroupID
    }
    deriving (Generic, FromJSON, ToSchema)

data UserUpdate = UserUpdate
    { newName :: Maybe Text
    , newEmail :: Maybe Text
    , newPassword :: Maybe Text
    , newRole :: Maybe (Group.GroupID, Text)
    }
    deriving (Generic, ToJSON, FromJSON, ToSchema)

{- | HasOpenApi instances for ProtectedAPI
  Copied from: https://github.com/biocad/servant-openapi3/issues/42
-}
instance (HasOpenApi api) => HasOpenApi (Auth '[] a :> api) where
    toOpenApi Proxy = toOpenApi $ Proxy @api

instance
    (HasOpenApi (Auth auths a :> api))
    => HasOpenApi (Auth (JWT : auths) a :> api)
    where
    toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Auth auths a :> api)
      where
        addSecurity =
            addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
        identifier :: Text = "JWT"
        securityScheme =
            SecurityScheme
                { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
                , _securitySchemeDescription = Just "Bearer Authentication"
                }

instance
    (HasOpenApi (Auth auths a :> api))
    => HasOpenApi (Auth (Cookie : auths) a :> api)
    where
    toOpenApi Proxy = addSecurity $ toOpenApi $ Proxy @(Auth auths a :> api)
      where
        addSecurity =
            addSecurityRequirement identifier . addSecurityScheme identifier securityScheme
        identifier :: Text = "JWT + XSRF-Cookie"
        securityScheme =
            SecurityScheme
                { _securitySchemeType = SecuritySchemeHttp $ HttpSchemeBearer $ Just "JWT"
                , _securitySchemeDescription = Just "Cookie Authentication"
                }

addSecurityScheme :: Text -> SecurityScheme -> OpenApi -> OpenApi
addSecurityScheme securityIdentifier securityScheme openApi =
    openApi
        { _openApiComponents =
            (_openApiComponents openApi)
                { _componentsSecuritySchemes =
                    _componentsSecuritySchemes (_openApiComponents openApi)
                        <> SecurityDefinitions (HM.singleton securityIdentifier securityScheme)
                }
        }

addSecurityRequirement :: Text -> OpenApi -> OpenApi
addSecurityRequirement securityRequirement =
    allOperations
        . security
        %~ ((SecurityRequirement $ HM.singleton securityRequirement []) :)