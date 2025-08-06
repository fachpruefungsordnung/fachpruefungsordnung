{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Pipeline (htmlPipeline) where

import Control.Applicative (empty)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Language.Lsd.Example.Fpo (sectionT)
import Language.Ltml.HTML (sectionToHtml)
import Language.Ltml.HTML.CSS (mainStylesheet)
import Language.Ltml.HTML.Util
import Language.Ltml.Parser.Section (sectionP)
import Lucid
import Servant (Handler)
import Text.Megaparsec (runParser)

-- | Parse section and render HTML with inlined CSS
htmlPipeline :: Text -> Handler ByteString
htmlPipeline input =
    case runParser (sectionP sectionT empty) "" input of
        Left _ -> return (renderBS errorHtml)
        Right nodeSection ->
            let body = sectionToHtml nodeSection
             in addInlineCssHeader "HTML Render Result" mainStylesheet

errorHtml :: Html ()
errorHtml = doctypehtml_ $ do
    head_ $ do
        title_ "Parsing Failed"
    body_ $ do
        h3_ "Parsing failed!"