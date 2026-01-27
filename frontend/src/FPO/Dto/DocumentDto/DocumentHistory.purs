-- | This module provides DTOs for document-wide history endpoints.
-- | It matches the swagger.json specification for /docs/{documentID}/history.

module FPO.Dto.DocumentDto.DocumentHistory
  ( DocumentHistory(..)
  , DocumentHistoryItem(..)
  , TreeRevisionHeader(..)
  , TreeRevisionHistory(..)
  , getDocumentID
  , getHistoryItems
  , isTreeItem
  , isTextItem
  , getItemHeader
  , getTextElementID
  , getTreeRevisionIdentifier
  , getTreeRevisionTimestamp
  , getTreeRevisionAuthor
  , getTreeHistoryDocumentID
  , getTreeHistoryItems
  ) where

import Prelude

import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import FPO.Dto.DocumentDto.DocDate (DocDate)
import FPO.Dto.DocumentDto.DocumentHeader (DocumentID, User)

-- | Header for tree revisions (document structure changes)
-- | Matches swagger TreeRevisionHeader schema
newtype TreeRevisionHeader = TRH
  { author :: User
  , identifier :: Int
  , timestamp :: DocDate
  }

derive newtype instance eqTreeRevisionHeader :: Eq TreeRevisionHeader

instance decodeJsonTreeRevisionHeader :: DecodeJson TreeRevisionHeader where
  decodeJson json = do
    obj <- decodeJson json
    author <- obj .: "author"
    identifier <- obj .: "identifier"
    timestamp <- obj .: "timestamp"
    pure $ TRH { author, identifier, timestamp }

getTreeRevisionIdentifier :: TreeRevisionHeader -> Int
getTreeRevisionIdentifier (TRH h) = h.identifier

getTreeRevisionTimestamp :: TreeRevisionHeader -> DocDate
getTreeRevisionTimestamp (TRH h) = h.timestamp

getTreeRevisionAuthor :: TreeRevisionHeader -> User
getTreeRevisionAuthor (TRH h) = h.author

-- | A document history item can be either a tree revision (structure change)
-- | or a text revision (content change in a specific text element).
-- | Matches swagger DocumentHistoryItem oneOf schema
data DocumentHistoryItem
  = TreeHistoryItem TreeRevisionHeader
  | TextHistoryItem
      { header :: TreeRevisionHeader -- TextRevisionHeader has same structure as TreeRevisionHeader
      , textElementID :: Int
      }

derive instance eqDocumentHistoryItem :: Eq DocumentHistoryItem

instance decodeJsonDocumentHistoryItem :: DecodeJson DocumentHistoryItem where
  decodeJson json = do
    obj <- decodeJson json
    itemType <- obj .: "type"
    case itemType of
      "tree" -> do
        header <- obj .: "header"
        pure $ TreeHistoryItem header
      "text" -> do
        header <- obj .: "header"
        textElementID <- obj .: "text"
        pure $ TextHistoryItem { header, textElementID }
      _ -> Left $ UnexpectedValue json

-- | Check if an item is a tree revision
isTreeItem :: DocumentHistoryItem -> Boolean
isTreeItem (TreeHistoryItem _) = true
isTreeItem (TextHistoryItem _) = false

-- | Check if an item is a text revision
isTextItem :: DocumentHistoryItem -> Boolean
isTextItem (TextHistoryItem _) = true
isTextItem (TreeHistoryItem _) = false

-- | Get the header from any history item
getItemHeader :: DocumentHistoryItem -> TreeRevisionHeader
getItemHeader (TreeHistoryItem h) = h
getItemHeader (TextHistoryItem { header }) = header

-- | Get the text element ID from a text history item (returns Nothing for tree items)
getTextElementID :: DocumentHistoryItem -> Maybe Int
getTextElementID (TreeHistoryItem _) = Nothing
getTextElementID (TextHistoryItem { textElementID }) = Just textElementID

-- | Document history response from /docs/{documentID}/history endpoint
-- | Matches swagger DocumentHistory schema
newtype DocumentHistory = DH
  { document :: DocumentID
  , history :: Array DocumentHistoryItem
  }

derive newtype instance eqDocumentHistory :: Eq DocumentHistory

instance decodeJsonDocumentHistory :: DecodeJson DocumentHistory where
  decodeJson json = do
    obj <- decodeJson json
    document <- obj .: "document"
    history <- obj .: "history"
    pure $ DH { document, history }

-- | Get the document ID from the history response
getDocumentID :: DocumentHistory -> DocumentID
getDocumentID (DH dh) = dh.document

-- | Get all history items from the history response
getHistoryItems :: DocumentHistory -> Array DocumentHistoryItem
getHistoryItems (DH dh) = dh.history

-- | Tree revision history response from /docs/{documentID}/tree/history endpoint
-- | Contains only tree revisions (document structure changes)
-- | Matches swagger TreeRevisionHistory schema
newtype TreeRevisionHistory = TRHist
  { document :: DocumentID
  , history :: Array TreeRevisionHeader
  }

derive newtype instance eqTreeRevisionHistory :: Eq TreeRevisionHistory

instance decodeJsonTreeRevisionHistory :: DecodeJson TreeRevisionHistory where
  decodeJson json = do
    obj <- decodeJson json
    document <- obj .: "document"
    history <- obj .: "history"
    pure $ TRHist { document, history }

-- | Get the document ID from the tree history response
getTreeHistoryDocumentID :: TreeRevisionHistory -> DocumentID
getTreeHistoryDocumentID (TRHist trh) = trh.document

-- | Get all tree revision headers from the tree history response
getTreeHistoryItems :: TreeRevisionHistory -> Array TreeRevisionHeader
getTreeHistoryItems (TRHist trh) = trh.history
