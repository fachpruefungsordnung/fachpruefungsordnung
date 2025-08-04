module FPO.Dto.DocumentDto where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError(..), decodeJson, (.:))
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(..))
import Data.Either (Either(..))
import Data.Enum (class BoundedEnum, toEnum)
import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Time (Time(..))
import Data.Tuple (fst)
import FPO.Dto.TreeDto (RootTree)
import Parsing (ParserT, fail, runParserT)
import Parsing.String (anyTill, char, rest)

type DocumentID = Int
type CommitID = Int

{- ---------------------- DocumentHeaderPlusPermission --------------------- -}

-- newtype DocumentHeaderPlusPermission = DHPP
--   { document :: NewDocumentHeader, documentPermission :: String }

-- getDHPPName :: DocumentHeaderPlusPermission -> String
-- getDHPPName (DHPP dhpp) = getNDHName dhpp.document

-- getDHPPID :: DocumentHeaderPlusPermission -> Int
-- getDHPPID (DHPP dhpp) = getNDHID dhpp.document

-- derive instance newtypeDocumentHeaderPlusPermission ::
--   Newtype DocumentHeaderPlusPermission _

-- instance decodeJsonHeaderPlusPermission :: DecodeJson DocumentHeaderPlusPermission where
--   decodeJson json = do
--     obj <- decodeJson json
--     doc <- obj .: "document"
--     docPerm <- obj .: "documentPermission"
--     pure $ DHPP { document: doc, documentPermission: docPerm }

{- ---------------------- User --------------------- -}

newtype User = U
  { identifier :: String, name :: String }

derive instance newtypeUser :: Newtype User _
derive newtype instance decodeJsonUser :: DecodeJson User

{- ---------------------- NewDocumentHeader --------------------- -}

newtype NewDocumentHeader = NDH
  { group :: Int
  , identifier :: DocumentID
  , lastEdited :: DocDate
  , lastEditedBy :: User
  , name :: String
  }

getNDHName :: NewDocumentHeader -> String
getNDHName (NDH ndh) = ndh.name

getNDHID :: NewDocumentHeader -> Int
getNDHID (NDH ndh) = ndh.identifier

getNDHLastEdited :: NewDocumentHeader -> DocDate
getNDHLastEdited (NDH ndh) = ndh.lastEdited

derive instance newtypeNewDocumentHeader :: Newtype NewDocumentHeader _
derive newtype instance decodeJsonNewDocumentHeader :: DecodeJson NewDocumentHeader

{- ---------------------- Query --------------------- -}

newtype Query = Q
  { group :: Maybe Int, user :: Maybe String }

derive instance newtypeQuery :: Newtype Query _
derive newtype instance decodeJsonQuery :: DecodeJson Query

newtype DocumentQuery = DQ
  { documents :: Array NewDocumentHeader, query :: Query }

getDQDocuments :: DocumentQuery -> Array NewDocumentHeader
getDQDocuments (DQ dq) = dq.documents

derive instance newtypeDocumentQuery :: Newtype DocumentQuery _
derive newtype instance decodeJsonDocumentQuery :: DecodeJson DocumentQuery

{- ---------------------- NodeHeader --------------------- -}

newtype NodeHeader = NodeHeader
  { identifier :: Int
  , kind :: String
  }

getNHId :: NodeHeader -> Int
getNHId (NodeHeader nh) = nh.identifier

derive instance newtypeNodeHeader :: Newtype NodeHeader _
derive newtype instance decodeJsonNodeHeader :: DecodeJson NodeHeader
derive newtype instance encodeJsonNodeHeader :: EncodeJson NodeHeader

derive instance genericNodeHeader :: Generic NodeHeader _
instance showNodeHeader :: Show NodeHeader where
  show = genericShow

{- ---------------------- DocDate --------------------- -}

newtype DocDate = DocDate DateTime

docDateToDateTime :: DocDate -> DateTime
docDateToDateTime (DocDate date) = date

timeParser :: forall m a. BoundedEnum a => Monad m => Char -> ParserT String m a
timeParser c = do
  res <- anyTill (char c)
  h res
  where
  h string = case fromString $ fst (string) of
    Nothing -> fail "can't parse number"
    Just num -> case toEnum num of
      Nothing -> fail "not valid"
      Just a -> pure a

dateParser :: forall m. Monad m => ParserT String m DateTime
dateParser = do
  year <- timeParser '-'
  month <- timeParser '-'
  day <- timeParser 'T'
  hour <- timeParser ':'
  minute <- timeParser ':'
  second <- timeParser '.'
  _ <- rest
  case (toEnum 0) of
    Nothing -> fail "not valid"
    Just g -> pure $ DateTime (canonicalDate year month day)
      (Time hour minute second g)

instance decodeJsonDateTime :: DecodeJson DocDate where
  decodeJson json = do
    obj <- decodeJson json
    result <- runParserT obj dateParser
    case result of
      Left _ -> Left (UnexpectedValue json)
      Right datetime -> Right $ DocDate datetime

derive newtype instance eqDocDate :: Eq DocDate
derive newtype instance ordDocDate :: Ord DocDate

{- ---------------------- DocumentTree --------------------- -}

type DocumentTree = RootTree NodeHeader

decodeDocument :: Json -> Either JsonDecodeError DocumentTree
decodeDocument json = do
  obj <- decodeJson json
  root <- obj .: "root"
  decodeJson root

encodeDocumentTree :: DocumentTree -> Json
encodeDocumentTree = encodeJson <<< map getNHId
