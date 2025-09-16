module FPO.Dto.DocumentDto.DocumentHeader
  ( DocumentHeader
  , DocumentID
  , getEditTimestamp
  , getID
  , getLastEdited
  , getName
  , getIdentifier
  , getUserName
  , User(..)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson)
import Data.DateTime (DateTime)
import FPO.Dto.DocumentDto.DocDate (DocDate, docDateToDateTime)

type DocumentID = Int

{- ----------------------------- User --------------------------- -}

newtype User = U
  { identifier :: String, name :: String }

derive newtype instance decodeJsonUser :: DecodeJson User
derive newtype instance eqUser :: Eq User

{- ---------------------- DocumentHeader --------------------- -}

newtype DocumentHeader = DH
  { group :: Int
  , created :: DocDate
  , createdBy :: User
  , identifier :: DocumentID
  , lastEdited :: DocDate
  , lastEditedBy :: User
  , name :: String
  }

derive newtype instance eqDocumentHeader :: Eq DocumentHeader

-- seems like it should be in one of the time files, but as it uses getLastEdited from here it would cause a dependency cycle if it were
getEditTimestamp ∷ DocumentHeader → DateTime
getEditTimestamp = docDateToDateTime <<< getLastEdited

getName :: DocumentHeader -> String
getName (DH dh) = dh.name

getUserName :: User -> String
getUserName (U u) = u.name

getID :: DocumentHeader -> Int
getID (DH dh) = dh.identifier

getLastEdited :: DocumentHeader -> DocDate
getLastEdited (DH dh) = dh.lastEdited

getIdentifier :: DocumentHeader -> DocumentID
getIdentifier (DH dh) = dh.identifier

derive newtype instance decodeJsonDocumentHeader :: DecodeJson DocumentHeader
