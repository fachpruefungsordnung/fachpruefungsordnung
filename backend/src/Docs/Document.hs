module Docs.Document
    ( DocumentID (..)
    , Document (..)
    ) where

import Data.Text (Text)
import GHC.Int (Int32)

newtype DocumentID = DocumentID
    { unDocumentID :: Int32
    }
    deriving (Eq)

data Document = Document
    { identifier :: DocumentID
    , name :: Text
    , group :: Int32
    }
