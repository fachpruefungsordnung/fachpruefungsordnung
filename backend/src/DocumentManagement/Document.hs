{-# LANGUAGE OverloadedStrings #-}

module DocumentManagement.Document
    ( Document (..)
    , DocumentID (..)
    , withNewDocumentHead
    ) where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict.InsOrd as InsOrd
import Data.OpenApi
    ( NamedSchema (..)
    , OpenApiType (..)
    , ToParamSchema (..)
    , ToSchema (..)
    , declareSchemaRef
    , exclusiveMinimum
    , minimum_
    , properties
    , required
    , type_
    )
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import DocumentManagement.Commit (CommitID)
import GHC.Int (Int32)
import UserManagement.Group (GroupID)
import Web.HttpApiData (FromHttpApiData (..))

-- | id type for documents
newtype DocumentID = DocumentID
    { unDocumentID :: Int32
    }
    deriving (Show, Eq, Ord)

instance ToJSON DocumentID where
    toJSON = toJSON . unDocumentID

instance FromJSON DocumentID where
    parseJSON = fmap DocumentID . parseJSON

instance ToSchema DocumentID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int32)

instance ToParamSchema DocumentID where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiInteger
            & minimum_ ?~ 0
            & exclusiveMinimum ?~ False

instance FromHttpApiData DocumentID where
    parseUrlPiece = (DocumentID <$>) . parseUrlPiece

-- | represents a document
data Document = Document
    { documentID :: DocumentID
    , documentName :: Text
    , documentGroupId :: GroupID
    , documentHead :: Maybe CommitID
    }
    deriving (Show)

instance ToJSON Document where
    toJSON (Document docID name group headCommit) =
        Aeson.object
            [ "id" .= docID
            , "name" .= name
            , "group" .= group
            , "headCommit" .= headCommit
            ]

instance FromJSON Document where
    parseJSON = Aeson.withObject "Document" $ \v ->
        Document
            <$> v .: "id"
            <*> v .: "name"
            <*> v .: "group"
            <*> v .: "headCommit"

instance ToSchema Document where
    declareNamedSchema _ = do
        idSchema <- declareSchemaRef (Proxy :: Proxy DocumentID)
        nameSchema <- declareSchemaRef (Proxy :: Proxy Text)
        groupIDSchema <- declareSchemaRef (Proxy :: Proxy Int32)
        commitSchema <- declareSchemaRef (Proxy :: Proxy (Maybe CommitID))
        return $
            NamedSchema (Just "Document") $
                mempty
                    & type_
                        ?~ OpenApiObject
                    & properties
                        .~ InsOrd.fromList
                            [ ("id", idSchema)
                            , ("name", nameSchema)
                            , ("group", groupIDSchema)
                            , ("headCommit", commitSchema)
                            ]
                    & required
                        .~ ["id", "name", "group"]

-- | Update the document head for the given document
withNewDocumentHead :: Document -> CommitID -> Document
withNewDocumentHead doc c = doc {documentHead = Just c}
