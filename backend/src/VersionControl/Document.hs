module VersionControl.Document (Document (..), DocumentID (..)) where

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
