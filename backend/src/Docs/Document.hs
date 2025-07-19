module Docs.Document
    ( DocumentID (..)
    , Document (..)
    ) where

import Data.Text (Text)
import GHC.Int (Int32)

newtype DocumentID = DocumentID
    { unDocumentID :: Int32
    }

data Document = Document
    { documentID :: DocumentID
    , documentName :: Text
    , documentGroup :: Int32
    }
