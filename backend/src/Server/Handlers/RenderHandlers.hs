{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Server.Handlers.RenderHandlers
    ( RenderAPI
    , renderServer
    , PDF
    , PDFByteString (..)
    , Zip
    , ZipByteString (..)
    ) where

import Control.Exception (Exception (displayException), SomeException, try)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (encode)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import Data.OpenApi
    ( NamedSchema (..)
    , ToSchema
    , binarySchema
    , declareNamedSchema
    )
import Data.Text (Text)
import Language.Ltml.HTML.Pipeline (htmlPipeline)
import Language.Ltml.ToLaTeX.PDFGenerator (generatePDFFromSection)
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

-- | PDF ByteString wrapper
newtype PDFByteString = PDFByteString ByteString

instance ToSchema PDFByteString where
    declareNamedSchema _ = pure $ NamedSchema (Just "PDF BinaryString") binarySchema

--

-- | Zip ByteString wrapper
newtype ZipByteString = ZipByteString ByteString

instance ToSchema ZipByteString where
    declareNamedSchema _ = pure $ NamedSchema (Just "Zip BinaryString") binarySchema

-- | API type for all render formats
type RenderAPI =
    "render"
        :> ( "html" :> RenderRoute HTML
                :<|> "plain" :> RenderRoute Plain
                :<|> "pdf"
                    :> Auth AuthMethod Auth.Token
                    :> ReqBody '[JSON] Text
                    :> Post '[PDF] PDFByteString
           )

renderServer :: Server RenderAPI
renderServer =
    renderHandler htmlPipeline :<|> renderHandler renderPlain :<|> renderPDFHandler

-- | Format type for HTML
data HTML

-- | Format type for plain text
data Plain

-- | Format type for PDF
data PDF

-- | Format type for Zip
data Zip

instance Accept Zip where
    contentType _ = "application" // "zip"

instance Accept PDF where
    contentType _ = "application" // "pdf"

-- | MIME type for HTML
instance Accept HTML where
    contentType _ = "text" // "html"

-- | MIME type for plain text
instance Accept Plain where
    contentType _ = "text" // "plain"

instance MimeRender Zip ZipByteString where
    mimeRender _ (ZipByteString bs) = bs

instance MimeRender PDF PDFByteString where
    mimeRender _ (PDFByteString bs) = bs

instance MimeRender Plain DocByteString where
    mimeRender _ (DocByteString bs) = bs

instance MimeRender HTML DocByteString where
    mimeRender _ (DocByteString bs) = bs

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
renderHandler renderFunc (Authenticated _) input = do
    return $ DocByteString $ renderFunc input
renderHandler _ _ _ = throwError errNotLoggedIn

renderPDFHandler
    :: AuthResult Auth.Token -> Text -> Handler PDFByteString
renderPDFHandler (Authenticated _) input = do
    result <- liftIO $ try $ generatePDFFromSection input
    case result of
        Left (e :: SomeException) -> do
            liftIO $ putStrLn ("*** Handler exception: " ++ displayException e)
            throwError err500 {errBody = BS.pack "Internal Server Error"}
        Right eAction -> case eAction of
            Left err -> throwError err400 {errBody = BS.pack err}
            Right pdf -> return $ PDFByteString pdf
renderPDFHandler _ _ = throwError errNotLoggedIn
