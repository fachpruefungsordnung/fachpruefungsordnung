{-# LANGUAGE DeriveGeneric #-}

module Docs.Comment
    ( CommentID (..)
    , Status (..)
    , Comment (..)
    , Message (..)
    , CommentRef (..)
    , CommentAnchor (..)
    , Range (start, end)
    , Anchor (Anchor, row, col)
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
import Data.Vector (Vector)
import Docs.TextElement (TextElementRef, prettyPrintTextElementRef)
import Docs.UserRef (UserRef)
import GHC.Generics (Generic)
import GHC.Int (Int64)
import Servant (FromHttpApiData (parseUrlPiece))

-- | References a comment of a @TextElement@
data CommentRef = CommentRef TextElementRef CommentID
    deriving (Generic)

instance ToJSON CommentRef

instance FromJSON CommentRef

instance ToSchema CommentRef

-- | Obtain a human readable representation of a @CommentRef@
prettyPrintCommentRef :: CommentRef -> String
prettyPrintCommentRef (CommentRef textElementRef id_) =
    prettyPrintTextElementRef textElementRef ++ show id_

-- | Unique identifier for a comment
newtype CommentID = CommentID
    { unCommentID :: Int64
    }
    deriving (Show, Eq, Ord)

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

-- | Wether a comment is still open or resolved
data Status
    = Open
    | Resolved UTCTime
    deriving (Generic)

instance ToJSON Status

instance FromJSON Status

instance ToSchema Status

-- | An existing comment present in the database
data Comment = Comment
    { identifier :: CommentID
    , status :: Status
    , message :: Message
    , replies :: Vector Message
    }
    deriving (Generic)

instance ToJSON Comment

instance FromJSON Comment

instance ToSchema Comment

-- | The message of a comment or a reply
data Message = Message
    { author :: UserRef
    , timestamp :: UTCTime
    , content :: Text
    }
    deriving (Generic)

instance ToJSON Message

instance FromJSON Message

instance ToSchema Message

-- | Ties a comment to a specific text passage
data CommentAnchor = CommentAnchor
    { comment :: CommentID
    , anchor :: Range
    }
    deriving (Generic, Eq, Ord)

instance ToJSON CommentAnchor

instance FromJSON CommentAnchor

instance ToSchema CommentAnchor

-- | Text location
data Anchor = Anchor
    { col :: Int64
    , row :: Int64
    }
    deriving (Generic, Eq)

instance ToJSON Anchor

instance FromJSON Anchor

instance ToSchema Anchor

instance Ord Anchor where
    compare a b = case compare (row a) (row b) of
        EQ -> compare (col a) (col b)
        ordering -> ordering

-- | Text passage
data Range = Range
    { start :: Anchor
    , end :: Anchor
    }
    deriving (Generic, Eq, Ord)

instance ToJSON Range

instance FromJSON Range

instance ToSchema Range

range :: Anchor -> Anchor -> Range
range a b
    | a <= b = Range a b
    | otherwise = Range b a
