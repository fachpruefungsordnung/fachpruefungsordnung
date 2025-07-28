{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.CSS.CSS () where

import Clay hiding (map)

import Data.Text.Lazy (unpack)
import Language.Ltml.HTML.CSS.Classes

writeCss :: IO ()
writeCss = writeFile "static/out.css" (unpack $ render mainStylesheet)

-- | List of all Css Classes defined in Language.Ltml.HTML.CSS.Classes
cssClasses :: [Css]
cssClasses = map classStyle [minBound .. maxBound]

mainStylesheet :: Css
mainStylesheet = do
    body ? do
        fontFamily ["Arial"] [sansSerif]

    mconcat cssClasses
