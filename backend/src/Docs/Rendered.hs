{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Docs.Rendered
    ( PDFBytes (..)
    , ZipBytes (..)
    , HTMLBytes (..)
    , PDF
    , Zip
    , HTML
    )
where

import Data.ByteString.Lazy (ByteString)
import Data.OpenApi (NamedSchema (NamedSchema), ToSchema (declareNamedSchema))
import Data.OpenApi.ParamSchema (binarySchema)
import Data.Text (Text)
import Network.HTTP.Media ((//))
import Servant (Accept (contentType), MimeRender (mimeRender))

newtype PDFBytes
    = PDFBytes
    { unPDFBytes :: ByteString
    }

newtype ZipBytes
    = ZipBytes
    { unZipBytes :: ByteString
    }

newtype HTMLBytes
    = HTMLBytes
    { unHTMLBytes :: ByteString
    }

instance ToSchema PDFBytes where
    declareNamedSchema _ = declareBinarySchema "PDF Bytes"

instance ToSchema ZipBytes where
    declareNamedSchema _ = declareBinarySchema "Zip Bytes"

instance ToSchema HTMLBytes where
    declareNamedSchema _ = declareBinarySchema "HTML Bytes"

declareBinarySchema :: (Applicative f) => Text -> f NamedSchema
declareBinarySchema name = pure $ NamedSchema (Just name) binarySchema

data PDF

data Zip

data HTML

instance Accept PDF where
    contentType _ = "application" // "pdf"

instance Accept Zip where
    contentType _ = "application" // "zip"

instance Accept HTML where
    contentType _ = "text" // "plain"

instance MimeRender PDF PDFBytes where
    mimeRender _ = unPDFBytes

instance MimeRender Zip ZipBytes where
    mimeRender _ = unZipBytes

instance MimeRender HTML HTMLBytes where
    mimeRender _ = unHTMLBytes
