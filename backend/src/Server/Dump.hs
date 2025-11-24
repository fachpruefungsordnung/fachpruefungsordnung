{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Dump (DumpAPI, dumpHandler) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.OpenApi (NamedSchema (NamedSchema), ToSchema (declareNamedSchema))
import Data.OpenApi.ParamSchema (binarySchema)
import Network.HTTP.Media ((//))
import Servant
import Servant.Types.SourceT
import System.Environment (getEnv)
import System.Process
    ( CreateProcess (env, std_out)
    , StdStream (CreatePipe)
    , createProcess
    , proc
    )

newtype SQLBytes
    = SQLBytes
    { unSQLBytes :: BL.ByteString
    }

instance ToSchema SQLBytes where
    declareNamedSchema _ = pure $ NamedSchema (Just "SQL Bytes") binarySchema

instance ToSourceIO BL.ByteString SQLBytes where
    toSourceIO (SQLBytes bs) = source $ Yield bs Stop

data SQL

instance Accept SQL where
    contentType _ = "text" // "plain"

instance MimeRender SQL SQLBytes where
    mimeRender _ = unSQLBytes

instance MimeRender SQL BL.ByteString where
    mimeRender _ = id

type DumpAPI =
    "dump"
        :> StreamGet
            NewlineFraming
            SQL
            SQLBytes

dumpHandler
    :: Handler SQLBytes
dumpHandler = liftIO $ do
    host <- getEnv "POSTGRES_HOST"
    port <- getEnv "POSTGRES_PORT"
    user <- getEnv "POSTGRES_USER"
    password <- getEnv "POSTGRES_PASSWORD"
    db <- getEnv "POSTGRES_DB"

    (_, mHout, _, _) <-
        createProcess
            (proc "pg_dump" ["-h", host, "-p", port, "-U", user, "-d", db])
                { std_out = CreatePipe
                , env = Just [("PGPASSWORD", password)]
                }

    hout <- case mHout of
        Just h -> return h
        Nothing -> error "Failed to create stdout pipe for pg_dump"

    dump <- BL.hGetContents hout

    return $ SQLBytes dump
