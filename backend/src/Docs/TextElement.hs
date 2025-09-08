{-# LANGUAGE DeriveGeneric #-}

module Docs.TextElement
    ( TextElementID (..)
    , TextElement (..)
    , TextElementKind
    , TextElementRef (..)
    , prettyPrintTextElementRef
    ) where

import Data.Proxy (Proxy (..))
import Data.Text (Text)

import GHC.Generics (Generic)
import GHC.Int (Int64)

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
import Web.HttpApiData (FromHttpApiData (..))

import Docs.Document (DocumentID (..))
import Docs.Hash (Hashable (..))

-- | ID for a text element
newtype TextElementID = TextElementID
    { unTextElementID :: Int64
    }
    deriving (Eq, Ord, Show)

instance Hashable TextElementID where
    updateHash ctx = updateHash ctx . unTextElementID

instance ToJSON TextElementID where
    toJSON = toJSON . unTextElementID

instance FromJSON TextElementID where
    parseJSON = fmap TextElementID . parseJSON

instance ToSchema TextElementID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToParamSchema TextElementID where
    toParamSchema _ =
        mempty
            & type_ ?~ OpenApiInteger
            & minimum_ ?~ 0
            & exclusiveMinimum ?~ False

instance FromHttpApiData TextElementID where
    parseUrlPiece = (TextElementID <$>) . parseUrlPiece

-- | Scoped identifier for a text element
data TextElementRef = TextElementRef
    { documentID :: DocumentID
    , textElementID :: TextElementID
    }
    deriving (Eq, Generic)

prettyPrintTextElementRef :: TextElementRef -> String
prettyPrintTextElementRef ref =
    show (unDocumentID (documentID ref))
        ++ "."
        ++ show (unTextElementID (textElementID ref))

instance ToJSON TextElementRef

instance FromJSON TextElementRef

instance ToSchema TextElementRef

type TextElementKind = Text

-- | Contains metadata about a text element.
data TextElement = TextElement
    { identifier :: TextElementID
    , kind :: TextElementKind
    }
    deriving (Generic)

instance ToJSON TextElement

instance FromJSON TextElement

instance ToSchema TextElement
