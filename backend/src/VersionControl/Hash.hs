{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VersionControl.Hash
    ( Hashable (..)
    , Hashed (..)
    , Hash (..)
    , hashed
    )
where

import Control.Lens ((.~), (?~))
import qualified Crypto.Hash.SHA1 as SHA1
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decode, encode)
import Data.ByteString.Char8 (pack)
import Data.Function ((&))
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( NamedSchema (..)
    , OpenApiType (..)
    , ToParamSchema (..)
    , ToSchema (..)
    , declareSchemaRef
    , properties
    , required
    , type_
    )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import GHC.Int
import Web.HttpApiData (FromHttpApiData (..))

-- | represents the hash of a value
newtype Hash = Hash
    { unHash :: ByteString
    }
    deriving (Show)

instance ToJSON Hash where
    toJSON (Hash bs) = Aeson.String $ TE.decodeUtf8 $ encode bs

instance FromJSON Hash where
    parseJSON = Aeson.withText "Hash" $ \t ->
        case decode (TE.encodeUtf8 t) of
            Right bs -> pure (Hash bs)
            Left err -> fail $ "Invalid base16 encoding in Hash: " ++ err

instance ToSchema Hash where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy String)

instance ToParamSchema Hash where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiString

instance FromHttpApiData Hash where
    parseUrlPiece t = case decode (TE.encodeUtf8 t) of
        Right bs -> Right (Hash bs)
        Left _ -> Left "Invalid base16 encoding in Hash"

-- | a hashable value
class Hashable a where
    updateHash :: SHA1.Ctx -> a -> SHA1.Ctx

    hash :: a -> Hash
    hash = Hash . SHA1.finalize . updateHash SHA1.init

instance Hashable ByteString where
    updateHash = SHA1.update
    hash = Hash . SHA1.hash

instance Hashable Hash where
    updateHash ctx (Hash bs) = updateHash ctx bs
    hash (Hash bs) = hash bs

instance Hashable Int where
    updateHash = updateHashShow

instance Hashable Int32 where
    updateHash = updateHashShow

instance (Hashable a) => Hashable [a] where
    updateHash = foldr $ flip updateHash

instance (Hashable a) => Hashable (Maybe a) where
    updateHash ctx (Just a) = updateHash ctx a
    updateHash ctx _ = ctx

instance Hashable Text where
    updateHash ctx text = updateHash ctx $ TE.encodeUtf8 text

-- | represents a value together with its hash
data Hashed a = Hashed Hash a deriving (Show)

instance (Hashable a, ToJSON a) => ToJSON (Hashed a) where
    toJSON (Hashed h content) =
        Aeson.object ["hash" .= h, "content" .= content]

instance (Hashable a, FromJSON a) => FromJSON (Hashed a) where
    parseJSON = Aeson.withObject "Hashed" $ \v ->
        Hashed
            <$> v .: "hash"
            <*> v .: "content"

instance (ToSchema a) => ToSchema (Hashed a) where
    declareNamedSchema _ = do
        hashSchema <- declareSchemaRef (Proxy :: Proxy Hash)
        contentSchema <- declareSchemaRef (Proxy :: Proxy a)
        return $
            NamedSchema (Just "Hashed") $
                mempty
                    & type_
                        ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("hash", hashSchema)
                            , ("content", contentSchema)
                            ]
                    & required
                        .~ ["hash", "content"]

-- | returns the input together with its hash
hashed :: (Hashable a) => a -> Hashed a
hashed x = Hashed (hash x) x

-- | update a hash by applying the value to 'show'
updateHashShow :: (Show a) => SHA1.Ctx -> a -> SHA1.Ctx
updateHashShow ctx = SHA1.update ctx . pack . show
