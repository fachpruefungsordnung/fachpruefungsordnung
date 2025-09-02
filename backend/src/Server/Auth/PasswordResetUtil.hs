{-# LANGUAGE OverloadedStrings #-}

module Server.Auth.PasswordResetUtil
    ( generateResetToken
    , hashToken
    , validateTokenFormat
    , createResetUrl
    , sendPasswordResetEmail
    , getTokenExpirationTime
    , TokenValidationResult (..)
    ) where

import Crypto.Hash.SHA1 (hash)
import qualified Data.ByteString as BS
import Data.ByteString.Base64 (decode, encode)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LT
import Data.Time
    ( UTCTime
    , addUTCTime
    , defaultTimeLocale
    , formatTime
    , getCurrentTime
    )
import qualified Mail
import Network.Mail.Mime (Address (..), htmlPart, plainPart)

-- | Result of token validation
data TokenValidationResult
    = TokenValid
    | TokenInvalid
    | TokenExpired
    | TokenAlreadyUsed
    deriving (Eq, Show)

-- | Generate a cryptographically secure random token
generateResetToken :: IO Text
generateResetToken = do
    -- Generate 32 random bytes (256 bits of entropy) using current time as seed
    now <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%s%q" now
    let tokenData = Text.encodeUtf8 $ Text.pack $ "reset_" <> timeStr <> "_token"
    return $ Text.decodeUtf8 $ encode tokenData

-- | Hash a token for secure storage
hashToken :: Text -> Text
hashToken token =
    let bytes = Text.encodeUtf8 token
        hashedBytes = hash bytes
     in Text.decodeUtf8 $ encode hashedBytes

-- | Validate token format (base64 encoded, appropriate length)
validateTokenFormat :: Text -> Bool
validateTokenFormat token =
    case decode (Text.encodeUtf8 token) of
        Left _ -> False
        Right decoded -> BS.length decoded > 10 && BS.length decoded < 200

-- | Create a password reset URL with the given token
createResetUrl :: Text -> Text -> Text
createResetUrl baseUrl token =
    baseUrl <> "/reset-password?token=" <> token

-- | Send password reset email to user
sendPasswordResetEmail :: Text -> Text -> Text -> IO ()
sendPasswordResetEmail userEmail userName resetUrl = do
    let mail =
            Mail.Mail
                { Mail.receiver = Address (Just userName) userEmail
                , Mail.subject = "Password Reset - Fachprüfungsordnung"
                , Mail.body =
                    [ plainPart (LT.fromStrict plainBody)
                    , htmlPart (LT.fromStrict htmlBody)
                    ]
                }
    Mail.sendMailTo mail
  where
    plainBody =
        Text.unlines
            [ "Hello " <> userName <> ","
            , ""
            , "You have requested a password reset for your Fachprüfungsordnung account."
            , ""
            , "Please click on the following link to reset your password:"
            , resetUrl
            , ""
            , "This link will expire in 1 hour for security reasons."
            , ""
            , "If you did not request this password reset, please ignore this email."
            , "Your password will remain unchanged."
            , ""
            , "Note: The UI is currently not implemented, so use the /password-reset/confirm endpoint manually (https://batailley.informatik.uni-kiel.de/swagger/)."
            , ""
            , "Best regards,"
            , "The Fachprüfungsordnung Team"
            ]

    htmlBody =
        Text.unlines
            [ "<html><body>"
            , "<h2>Password Reset Request</h2>"
            , "<p>Hello <strong>" <> userName <> "</strong>,</p>"
            , "<p>You have requested a password reset for your Fachprüfungsordnung account.</p>"
            , "<p>Please click on the button below to reset your password:</p>"
            , "<div style='margin: 20px 0;'>"
            , "  <a href='"
                <> resetUrl
                <> "' style='background-color: #007bff; color: white; padding: 12px 24px; text-decoration: none; border-radius: 4px; display: inline-block;'>Reset Password</a>"
            , "</div>"
            , "<p>Or copy and paste this link into your browser:</p>"
            , "<p><a href='" <> resetUrl <> "'>" <> resetUrl <> "</a></p>"
            , "<p><strong>Important:</strong> This link will expire in 1 hour for security reasons.</p>"
            , "<p>If you did not request this password reset, please ignore this email. Your password will remain unchanged.</p>"
            , "<hr>"
            , "<p><small>Best regards,<br>The Fachprüfungsordnung Team</small></p>"
            , "</body></html>"
            ]

-- | Calculate token expiration time (1 hour from now)
getTokenExpirationTime :: IO UTCTime
getTokenExpirationTime = addUTCTime (60 * 60) <$> getCurrentTime
