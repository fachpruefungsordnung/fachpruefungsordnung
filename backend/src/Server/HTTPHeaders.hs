{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Server.HTTPHeaders () where

data CacheControl = NoCache | MaxAge Int

instance Show CacheControl where
    show NoCache = "no-cache"
    show (MaxAge s) = "max-age=" ++ show s
