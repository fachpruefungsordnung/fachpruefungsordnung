{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTPHeaders (PDF) where

import Data.ByteString.Lazy (ByteString)
import Network.HTTP.Media.MediaType ((//))
import Servant.API

-- PDF MIME type
data PDF

instance Accept PDF where
    contentType _ = "application" // "pdf"

instance MimeRender PDF ByteString where
    mimeRender _ = id

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
