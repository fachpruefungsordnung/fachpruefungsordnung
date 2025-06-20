{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.HTTPHeaders (PDF, PDFByteString (..), HTML) where

import Data.ByteString.Lazy (ByteString)
import Data.OpenApi (NamedSchema (..), ToSchema (..), binarySchema)
import Network.HTTP.Media.MediaType ((//))
import Servant.API

-- | PDF ByteString wrapper
newtype PDFByteString = PDFByteString ByteString

instance ToSchema PDFByteString where
    declareNamedSchema _ = pure $ NamedSchema (Just "PDF BinaryString") binarySchema

-- | PDF MIME type
data PDF

instance Accept PDF where
    contentType _ = "application" // "pdf"

instance MimeRender PDF PDFByteString where
    mimeRender _ (PDFByteString bs) = bs

-- | HTML MIME type
data HTML

instance Accept HTML where
    contentType _ = "text" // "html"

-- Cache-Control Headers
type HeaderCacheControl = Header "Cache-Control" String

data CacheControl = NoCache | MaxAge Int

minutes5 :: Int
minutes5 = 300

instance Show CacheControl where
    show NoCache = "no-cache"
    show (MaxAge s) = "max-age=" ++ show s

addCacheControl
    :: (AddHeader '[Optional, Strict] "Cache-Control" String orig new)
    => CacheControl
    -> orig
    -> new
addCacheControl cacheControl = addHeader (show cacheControl)
