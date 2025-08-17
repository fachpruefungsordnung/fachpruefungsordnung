{-# LANGUAGE DeriveGeneric #-}

module Docs.Comment
    ( CommentID (..)
    , Status (..)
    , Comment (..)
    , CommentRef (..)
    , CommentAnchor (..)
    , Range (start, end)
    , range
    , prettyPrintCommentRef
    ) where

import Control.Lens ((&))
import Control.Lens.Operators ((?~))
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.OpenApi
    ( HasExclusiveMinimum (exclusiveMinimum)
    , HasMinimum (minimum_)
    , OpenApiType (OpenApiInteger)
    , ToParamSchema (toParamSchema)
    , ToSchema (declareNamedSchema)
    )
import Data.OpenApi.Lens (HasType (..))
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Time (UTCTime)
import Docs.TextElement (TextElementRef, prettyPrintTextElementRef)
import Docs.UserRef (UserRef)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Servant (FromHttpApiData (parseUrlPiece))

data CommentRef = CommentRef TextElementRef CommentID

prettyPrintCommentRef :: CommentRef -> String
prettyPrintCommentRef (CommentRef textElementRef id_) =
    prettyPrintTextElementRef textElementRef ++ show id_

newtype CommentID = CommentID
    { unCommentID :: Int64
    }
    deriving (Show)

instance ToJSON CommentID where
    toJSON = toJSON . unCommentID

instance FromJSON CommentID where
    parseJSON = fmap CommentID . parseJSON

instance ToSchema CommentID where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToParamSchema CommentID where
    toParamSchema _ =
        mempty
            & type_
                ?~ OpenApiInteger
            & minimum_
                ?~ 0
            & exclusiveMinimum
                ?~ False

instance FromHttpApiData CommentID where
    parseUrlPiece = (CommentID <$>) . parseUrlPiece

data Status
    = Open
    | Resolved UTCTime
    deriving (Generic)

instance ToJSON Status

instance FromJSON Status

instance ToSchema Status

data Comment = Comment
    { identifier :: CommentID
    , author :: UserRef
    , timestamp :: UTCTime
    , status :: Status
    , content :: Text
    }
    deriving (Generic)

instance ToJSON Comment

instance FromJSON Comment

instance ToSchema Comment

data CommentAnchor = CommentAnchor
    { comment :: CommentID
    , anchor :: Range
    }
    deriving (Generic)

instance ToJSON CommentAnchor

instance FromJSON CommentAnchor

instance ToSchema CommentAnchor

data Range = Range
    { start :: Int64
    , end :: Int64
    }
    deriving (Generic)

instance ToJSON Range

instance FromJSON Range

instance ToSchema Range

range :: Int64 -> Int64 -> Range
range a b
    | a <= b = Range a b
    | otherwise = Range b a
