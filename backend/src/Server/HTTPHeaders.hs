{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.HTTPHeaders () where

import Servant.API

-- HTTP Cache-Control Headers
type HeaderCacheControl = Header "Cache-Control" String

data CacheControl = NoCache | MaxAge Int

instance Show CacheControl where
    show NoCache = "no-cache"
    show (MaxAge s) = "max-age=" ++ show s

addCacheControl
    :: (AddHeader '[Optional, Strict] "Cache-Control" String orig new)
    => CacheControl
    -> orig
    -> new
addCacheControl cacheControl = addHeader (show cacheControl)
