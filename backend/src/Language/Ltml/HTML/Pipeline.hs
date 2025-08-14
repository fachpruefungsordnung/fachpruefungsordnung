{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Pipeline (htmlPipeline) where

import Control.Applicative (empty)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Language.Lsd.Example.Fpo (sectionT)
import Language.Ltml.HTML (renderHtmlCss)
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Language.Ltml.HTML.CSS.Util
import Language.Ltml.Parser.Section (sectionP)
import Lucid
import Text.Megaparsec (runParser)

-- | Parse section and render HTML with inlined CSS
htmlPipeline :: Text -> ByteString
htmlPipeline input =
    case runParser (sectionP sectionT empty) "" input of
        Left err -> renderBS $ errorHtml (show err)
        Right nodeSection ->
            let (body, css) = renderHtmlCss nodeSection
             in renderBS $ addInlineCssHeader css body

-------------------------------------------------------------------------------

-- | Takes error message and generates error html
errorHtml :: String -> Html ()
errorHtml err = doctypehtml_ $ do
    -- head_ $
    --     style_ (toStrict $ render (Class.classStyle Class.Document))
    body_ $ do
        div_ <#> Class.Document $ do
            h3_ "Parsing failed!"
            p_ $ toHtml err
