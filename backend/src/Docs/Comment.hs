module Docs.Comment
    ( CommentID (..)
    , Status (..)
    , Comment (..)
    , AnchoredComment (..)
    , Range (start, end)
    , range
    ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Docs.UserRef (UserRef)
import GHC.Int (Int64)

newtype CommentID = CommentID
    { unCommentID :: Int64
    }

data Status = Open | Resolved UTCTime

data Comment = Comment
    { identifier :: CommentID
    , author :: UserRef
    , timestamp :: UTCTime
    , status :: Status
    , content :: Text
    }

data AnchoredComment = AnchoredComment
    { comment :: Comment
    , location :: Range
    }

data Range = Range
    { start :: Int64
    , end :: Int64
    }

range :: Int64 -> Int64 -> Maybe Range
range a b
    | a <= b = Just $ Range a b
    | otherwise = Nothing
