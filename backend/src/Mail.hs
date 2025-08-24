{-# LANGUAGE OverloadedStrings #-}

module Mail (testMail, Mail (..), MailSettings (..), sendMailTo, sendMailTo') where

import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as Text
import Network.Mail.Mime (Part, htmlPart, plainPart)
import Network.Mail.SMTP
    ( Address (Address)
    , sendMailWithLoginTLS'
    , simpleMail
    )
import Network.Socket (PortNumber)
import System.Environment (getEnv)

data MailSettings = MailSettings
    { host :: String
    , port :: PortNumber
    , username :: String
    , address :: Text
    , password :: String
    }
    deriving (Show)

data Mail = Mail
    { receiver :: Address
    , subject :: Text
    , body :: [Part]
    }
    deriving (Show)

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
    sendMailTo' settings mail

sendMailTo' :: MailSettings -> Mail -> IO ()
sendMailTo' settings mail = do
    print settings
    print mail
    sendMailWithLoginTLS'
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
