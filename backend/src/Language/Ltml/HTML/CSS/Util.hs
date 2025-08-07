{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.CSS.Util
    ( (<#>)
    , cssClass_
    , addHtmlHeader
    , addInlineCssHeader
    ) where

import Clay (Css, render)
import Data.Text (pack)
import Data.Text.Lazy (toStrict)
import Language.Ltml.HTML.CSS.Classes (Class, className)
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Lucid

-- | Constructs HTML element with given Class
(<#>) :: ([Attributes] -> a) -> Class -> a
htmlFunc <#> cssClass = htmlFunc [class_ (className cssClass)]

-- | Convert CSS Class to Lucid HTML Attribute
cssClass_ :: Class -> Attributes
cssClass_ = class_ . className

-------------------------------------------------------------------------------

-- | Adds html, head and body tags onto given html and
--   sets title and css path
addHtmlHeader :: String -> FilePath -> Html () -> Html ()
addHtmlHeader title cssPath html = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        link_ [rel_ "stylesheet", href_ (pack cssPath)]
    body_ $ div_ <#> Class.Document $ html

-- | Adds html, head and body tags onto given html,
--   renders and inlines given css;
--   This is used for creating a "preview" HTML;
addInlineCssHeader :: Css -> Html () -> Html ()
addInlineCssHeader css html =
    doctypehtml_ $ do
        head_ $ do
            style_ (toStrict $ render css)
        body_ $ div_ <#> Class.Document $ html
