{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.CSS.Util
    ( -- * Convert CSS Classes to HTML Attributes
      (<#>)
    , cssClass_
    , cssClasses_

      -- * Wrap HTML Headers and Stylesheets
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

infixl 9 <#>

-- | Convert CSS Class to Lucid HTML Attribute
cssClass_ :: Class -> Attributes
cssClass_ = class_ . className

-- | Convert List of CSS Classes to List of Lucid HTML Attribute
--   Note: Lucid combines list of class_ attributes so single HTML
--   class attribute.
cssClasses_ :: [Class] -> [Attributes]
cssClasses_ = map (class_ . className)

-------------------------------------------------------------------------------

-- | Adds html, @<head>@ and @<body>@ tags onto given html and
--   sets title and css path
addHtmlHeader :: (ToHtml title) => title -> FilePath -> Html () -> Html ()
addHtmlHeader title cssPath html = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        link_ [rel_ "stylesheet", href_ (pack cssPath)]
    body_ $ div_ <#> Class.Body $ html

-- | Adds html, head and body tags onto given html,
--   adds title, renders and inlines given css;
--   This is used for creating a "preview" HTML;
addInlineCssHeader :: String -> Css -> Html () -> Html ()
addInlineCssHeader title css html =
    doctypehtml_ $ do
        head_ $ do
            title_ (toHtml title)
            style_ (toStrict $ render css)
        body_ $ div_ <#> Class.Body $ html
