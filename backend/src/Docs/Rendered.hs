{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Docs.Rendered
-- Description : Types for Rendered Content
-- License     : AGPL-3
-- Maintainer  : stu235271@mail.uni-kiel.de
--               stu236925@mail.uni-kiel.de
--
-- This module contains types for rendered content (PDF, HTML, Zip).
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

-- | An in-memory PDF file
newtype PDFBytes
    = PDFBytes
    { unPDFBytes :: ByteString
    }

-- | An in-memory Zip file
newtype ZipBytes
    = ZipBytes
    { unZipBytes :: ByteString
    }

-- | An in-memory HTML file
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

-- | PDF MIME Type
data PDF

-- | Zip MIME Type
data Zip

-- | HTML MIME Type
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
