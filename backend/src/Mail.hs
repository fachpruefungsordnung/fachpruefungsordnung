{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Mail (testMail, Mail (..), MailSettings (..), sendMailTo, sendMailTo') where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON))
import qualified Data.Aeson as Aeson
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text
import Database (getConnection)
import Docs (logMessage)
import Docs.Hasql.Database (run)
import GHC.Generics (Generic)
import qualified Logging.Logs as Logs
import qualified Logging.Scope as Scope
import Network.Mail.Mime (Part, htmlPart, plainPart)
import Network.Mail.SMTP
    ( Address (Address)
    , sendMailWithLoginSTARTTLS'
    , simpleMail
    )
import Network.Socket (PortNumber)
import Parse (nonEmptyString, nonEmptyText)
import System.Environment (getEnv)

data Error
    = InvalidMailSettings MailSettings
    | ErrorSendingMail MailSettings Mail
    deriving (Generic)

instance ToJSON Error

data MailSettings = MailSettings
    { host :: String
    , port :: PortNumber
    , username :: String
    , address :: Text
    , password :: String
    }
    deriving (Show)

instance ToJSON MailSettings where
    toJSON (MailSettings {..}) =
        Aeson.object
            [ "host" .= host
            , "port" .= show port
            , "username" .= username
            , "address" .= address
            , "password" .= ('*' <$ password)
            ]

data Mail = Mail
    { receiver :: Address
    , subject :: Text
    , body :: [Part]
    }
    deriving (Show)

instance ToJSON Mail where
    toJSON (Mail {..}) =
        Aeson.object
            [ "receiver" .= show receiver
            , "subject" .= subject
            , "body" .= (show <$> body)
            ]

testMail :: IO ()
testMail =
    sendMailTo
        Mail
            { receiver = Address (Just "Fick Anyhow") "finn.evers@outlook.de"
            , subject = "Hallo, kennen Sie thiserror?"
            , body = [plainPart "email body", htmlPart "<h1>HTML</h1>"]
            }

sendMailTo :: Mail -> IO ()
sendMailTo mail = do
    settings <- envSettings
    case completeSettings settings of
        Just _ -> sendMailTo' settings mail
        Nothing -> do
            Right db <- getConnection
            _ <-
                run
                    (logMessage Logs.Error Nothing Scope.email $ InvalidMailSettings settings)
                    db
            return ()

sendMailTo' :: MailSettings -> Mail -> IO ()
sendMailTo' settings mail = do
    print settings
    print mail
    sendMailWithLoginSTARTTLS'
        (host settings)
        (port settings)
        (username settings)
        (password settings)
        mail'
  where
    from = Address (Just "Der Fachprüfungsordner") (address settings)
    to = [receiver mail]
    -- body = plainPart "email body"
    -- html = htmlPart "<h1>HTML</h1>"
    mail' = simpleMail from to [] [] (subject mail) (body mail)

completeSettings :: MailSettings -> Maybe MailSettings
completeSettings x@(MailSettings {..}) = do
    _ <- nonEmptyString host
    _ <- nonEmptyText address
    _ <- nonEmptyString username
    _ <- nonEmptyString password
    pure x

envSettings :: IO MailSettings
envSettings = do
    host' <- getEnv "MAIL_HOST"
    port' <- getEnv "MAIL_PORT" <&> read
    address' <- getEnv "MAIL_ADDRESS"
    username' <- getEnv "MAIL_USERNAME"
    password' <- getEnv "MAIL_PASSWORD"
    return $
        MailSettings
            { host = host'
            , port = port'
            , username = username'
            , address = Text.pack address'
            , password = password'
            }
