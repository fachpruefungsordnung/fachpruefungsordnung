{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.RenderHandlers (RenderAPI, renderServer, PDF, PDFByteString(..)) where

import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
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
import Language.Ltml.ToLaTeX (generatePDFFromSection)
import Control.Monad.IO.Class (MonadIO(liftIO))

-- | Return type for rendered documents
newtype DocByteString = DocByteString ByteString

instance ToSchema DocByteString where
    declareNamedSchema _ = pure $ NamedSchema (Just "Document BinaryString") binarySchema

-- | PDF ByteString wrapper
newtype PDFByteString = PDFByteString ByteString

instance ToSchema PDFByteString where
    declareNamedSchema _ = pure $ NamedSchema (Just "PDF BinaryString") binarySchema



-- | API type for all render formats
type RenderAPI =
    "render"
        :> ( "html" :> RenderRoute HTML
        :<|> "plain" :> RenderRoute Plain
        :<|> "pdf" :> Auth AuthMethod Auth.Token
                   :> ReqBody '[JSON] Text
                   :> Post '[PDF] PDFByteString
           )

renderServer :: Server RenderAPI
renderServer = renderHandler renderHTML :<|> renderHandler renderPlain :<|> renderPDFHandler

-- | Format type for HTML
data HTML

-- | Format type for plain text
data Plain

-- | Format type for PDF
data PDF

instance Accept PDF where
    contentType _ = "application" // "pdf"

-- | MIME type for HTML
instance Accept HTML where
    contentType _ = "text" // "html"

-- | MIME type for plain text
instance Accept Plain where
    contentType _ = "text" // "plain"

instance MimeRender PDF PDFByteString where
    mimeRender _ (PDFByteString bs) = bs

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

renderPDFHandler
    :: AuthResult Auth.Token -> Text -> Handler PDFByteString
renderPDFHandler (Authenticated _) input = do
    eAction <- liftIO $ generatePDFFromSection input
    case eAction of
        Left err -> throwError err500 { errBody = BS.pack err }
        Right pdf -> return $ PDFByteString pdf
renderPDFHandler _ _ = throwError errNotLoggedIn