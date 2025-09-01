module FPO.Dto.DocumentDto.DocumentHeader
  ( DocumentHeader
  , DocumentID
  , getID
  , getLastEdited
  , getName
  , getIdentifier
  , User
  ) where

import Prelude

import Data.Argonaut (class DecodeJson)
import FPO.Dto.DocumentDto.DocDate (DocDate)

type DocumentID = Int

{- ----------------------------- User --------------------------- -}

newtype User = U
  { identifier :: String, name :: String }

derive newtype instance decodeJsonUser :: DecodeJson User
derive newtype instance eqUser :: Eq User

{- ---------------------- DocumentHeader --------------------- -}

newtype DocumentHeader = DH
  { group :: Int
  , identifier :: DocumentID
  , lastEdited :: DocDate
  , lastEditedBy :: User
  , name :: String
  }

derive newtype instance eqDocumentHeader :: Eq DocumentHeader

getName :: DocumentHeader -> String
getName (DH dh) = dh.name

getID :: DocumentHeader -> Int
getID (DH dh) = dh.identifier

getLastEdited :: DocumentHeader -> DocDate
getLastEdited (DH dh) = dh.lastEdited

getIdentifier :: DocumentHeader -> DocumentID
getIdentifier (DH dh) = dh.identifier

derive newtype instance decodeJsonDocumentHeader :: DecodeJson DocumentHeader
