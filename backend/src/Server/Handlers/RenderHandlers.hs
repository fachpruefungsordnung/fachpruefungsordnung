{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.RenderHandlers (RenderAPI, renderServer) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import Data.OpenApi
    ( NamedSchema (..)
    , ToSchema
    , binarySchema
    , declareNamedSchema
    )
import Data.Text (Text, lines, unlines)
import Lucid
import Network.HTTP.Media.MediaType ((//))
import Servant
import Servant.Auth.Server
import Server.Auth (AuthMethod)
import qualified Server.Auth as Auth
import Server.HandlerUtil
import Prelude hiding (head, lines, unlines)

-- | Return type for rendered documents
newtype DocByteString = DocByteString ByteString

instance ToSchema DocByteString where
    declareNamedSchema _ = pure $ NamedSchema (Just "Document BinaryString") binarySchema

-- | API type for all render formats
type RenderAPI =
    "render"
        :> ( "html" :> RenderRoute HTML
                :<|> "plain" :> RenderRoute Plain
           )

renderServer :: Server RenderAPI
renderServer = renderHandler renderHTML :<|> renderHandler renderPlain

-- | Format type for HTML
data HTML

-- | Format type for plain text
data Plain

-- | MIME type for HTML
instance Accept HTML where
    contentType _ = "text" // "html"

-- | MIME type for plain text
instance Accept Plain where
    contentType _ = "text" // "plain"

instance MimeRender Plain DocByteString where
    mimeRender _ (DocByteString bs) = bs

instance MimeRender HTML DocByteString where
    mimeRender _ (DocByteString bs) = bs

renderHTML :: Text -> ByteString
renderHTML input =
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

renderPlain :: Text -> ByteString
renderPlain = encode

-- | Generic API type for single render format
type RenderRoute format =
    Auth AuthMethod Auth.Token
        :> ReqBody '[JSON] Text
        :> Post '[format] DocByteString

-- | Generic renderHandler which takes a render function
renderHandler
    :: (a -> ByteString) -> AuthResult Auth.Token -> a -> Handler DocByteString
renderHandler renderFunc (Authenticated _) input = return $ DocByteString $ renderFunc input
renderHandler _ _ _ = throwError errNotLoggedIn
