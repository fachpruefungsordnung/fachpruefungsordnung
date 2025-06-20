{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.RenderHandlers (RenderAPI, renderServer) where

import Data.Aeson (FromJSON)
import Data.ByteString.Lazy (ByteString)
import Data.OpenApi
    ( NamedSchema (..)
    , ToParamSchema
    , ToSchema
    , binarySchema
    , declareNamedSchema
    )
import Data.Text (Text, lines, toLower, unlines)
import GHC.Generics (Generic)
import Lucid
import Servant
import Servant.Auth.Server
import Server.Auth (AuthMethod)
import qualified Server.Auth as Auth
import Server.HTTPHeaders
import Server.HandlerUtil
import Prelude hiding (head, lines, unlines)

type RenderAPI =
    Auth AuthMethod Auth.Token
        :> "render"
        :> Capture "format" OutputFormat
        :> ReqBody '[JSON] Text
        :> Post '[OctetStream] (Headers '[HeaderContentType] DocByteString)

renderServer :: Server RenderAPI
renderServer = renderHandler

newtype DocByteString = DocByteString ByteString

instance ToSchema DocByteString where
    declareNamedSchema _ = pure $ NamedSchema (Just "Document BinaryString") binarySchema

instance MimeRender OctetStream DocByteString where
    mimeRender _ (DocByteString bs) = bs

data OutputFormat = HTML
    deriving (Eq, Generic, FromJSON, ToParamSchema)

formatToHeader :: OutputFormat -> String
formatToHeader format = case format of
    HTML -> "text/html" -- PDF -> "application/pdf"

instance FromHttpApiData OutputFormat where
    parseUrlPiece text = case toLower text of
        "html" -> Right HTML
        _ -> Left "Output format is not supported." -- Error msg send if undefined format is requested

renderHandler
    :: AuthResult Auth.Token
    -> OutputFormat
    -> Text
    -> Handler (Headers '[HeaderContentType] DocByteString)
renderHandler (Authenticated Auth.Token {..}) format input = case format of
    HTML ->
        return $ addHeader (formatToHeader HTML) $ DocByteString $ renderToHTML input
renderHandler _ _ _ = throwError errNotLoggedIn

renderToHTML :: Text -> ByteString
renderToHTML input =
    let (head, body) = case lines input of
            [] -> ("", "")
            (x : xs) -> (x, unlines xs)
     in do
            renderBS $
                html_ $ do
                    head_ $
                        title_ "Render Result"
                    body_ $ do
                        h1_ (toHtml head)
                        p_ (toHtml body)
