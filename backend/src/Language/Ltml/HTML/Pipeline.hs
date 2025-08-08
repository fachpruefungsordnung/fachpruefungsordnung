{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Pipeline (htmlPipeline) where

import Control.Applicative (empty)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Language.Lsd.Example.Fpo (sectionT)
import Language.Ltml.HTML (aToHtml)
import Language.Ltml.HTML.CSS (mainStylesheet)
import Language.Ltml.HTML.CSS.Util
import Language.Ltml.Parser.Section (sectionP)
import Lucid
import Text.Megaparsec (runParser)

-- | Parse section and render HTML with inlined CSS
htmlPipeline :: Text -> ByteString
htmlPipeline input =
    case runParser (sectionP sectionT empty) "" input of
        Left _ -> renderBS errorHtml
        Right nodeSection ->
            let body = aToHtml nodeSection
             in renderBS $ addInlineCssHeader mainStylesheet body

errorHtml :: Html ()
errorHtml = doctypehtml_ $ do
    body_ $ do
        h3_ "Parsing failed!"
