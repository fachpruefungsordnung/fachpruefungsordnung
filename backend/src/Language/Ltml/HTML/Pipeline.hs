{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Pipeline (htmlPipeline) where

import Clay (render)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Language.Lsd.Example.Fpo (footnoteT, sectionT)
import Language.Ltml.HTML (renderSectionHtmlCss)
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Language.Ltml.HTML.CSS.Util
import Language.Ltml.Parser.Common.Lexeme (nSc)
import Language.Ltml.Parser.Footnote (unwrapFootnoteParser)
import Language.Ltml.Parser.Section (sectionP)
import Lucid
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, runParser)

-- | Parse section and render HTML with inlined CSS
htmlPipeline :: Text -> ByteString
htmlPipeline input =
    case runParser
        (nSc *> unwrapFootnoteParser [footnoteT] (sectionP sectionT eof))
        ""
        (input <> "\n") of
        Left err -> renderBS $ errorHtml (errorBundlePretty err)
        Right (nodeSection, footnoteMap) ->
            let (body, css) = renderSectionHtmlCss nodeSection footnoteMap
             in renderBS $ addInlineCssHeader css body

-------------------------------------------------------------------------------

-- | Takes error message and generates error html
errorHtml :: String -> Html ()
errorHtml err = doctypehtml_ $ do
    head_ $
        style_
            ( toStrict $
                render
                    ( Class.classStyle Class.Body
                        <> Class.classStyle Class.DocumentTitle
                        <> Class.classStyle Class.LargeFontSize
                    )
            )
    body_ $ do
        div_ <#> Class.Body $ do
            h1_ <#> Class.DocumentTitle $ "Parsing failed!"
            pre_ $ code_ <#> Class.LargeFontSize $ toHtml err
