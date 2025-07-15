{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.CSS.CSS () where

import Clay
import Data.String (fromString)
import qualified Data.Text as T
import Data.Text.Lazy (unpack)
import qualified Language.Ltml.HTML.CSS.ClassNames as Class

writeCss :: IO ()
writeCss = writeFile "static/out.css" (unpack $ render mainStylesheet)

mainStylesheet :: Css
mainStylesheet = do
    body ? do
        fontFamily ["Arial"] [sansSerif]

    toClassSelector Class.underlined ? do
        textDecoration underline

toClassSelector :: T.Text -> Selector
toClassSelector s = fromString ("." ++ T.unpack s)