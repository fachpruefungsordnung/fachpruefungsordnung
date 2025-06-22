module VersionControl.Document
    ( Document (..)
    , DocumentID (..)
    , withNewDocumentHead
    ) where

import Data.Text (Text)
import GHC.Int (Int32)
import UserManagement.Group (GroupID)
import VersionControl.Commit (CommitID)

newtype DocumentID = DocumentID
    {unDocumentID :: Int32}

data Document = Document
    { documentID :: DocumentID
    , documentName :: Text
    , groupId :: GroupID
    , documentHead :: Maybe CommitID
    }

-- | Update the document head for the given document
withNewDocumentHead :: Document -> CommitID -> Document
withNewDocumentHead doc c = doc {documentHead = Just c}
