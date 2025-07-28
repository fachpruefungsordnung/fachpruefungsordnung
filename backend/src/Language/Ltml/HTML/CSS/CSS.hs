{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.CSS.CSS (writeCss) where

import Clay hiding (map)

import Data.Text.Lazy (unpack)
import Language.Ltml.HTML.CSS.Classes

writeCss :: FilePath -> IO ()
writeCss path = writeFile path (unpack $ render mainStylesheet)

-- | List of all Css Classes defined in Language.Ltml.HTML.CSS.Classes
cssClasses :: [Css]
cssClasses = map classStyle [minBound .. maxBound]

mainStylesheet :: Css
mainStylesheet = do
    body ? do
        fontFamily ["Arial"] [sansSerif]

    mconcat cssClasses
