{-# LANGUAGE DeriveGeneric #-}

module Server.DTOs.Logs (Logs (..)) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (ToSchema)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import GHC.Int (Int64)
import Logging.Logs (LogMessage)

data Logs = Logs
    { messages :: Vector LogMessage
    , offset :: Maybe UTCTime
    , limit :: Int64
    }
    deriving (Generic)

instance ToJSON Logs

instance FromJSON Logs

instance ToSchema Logs
