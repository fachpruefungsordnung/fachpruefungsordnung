{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.CSS.CSS (writeCss) where

import Clay hiding (map)

import Data.Text.Lazy.IO (writeFile)
import Language.Ltml.HTML.CSS.Classes
import Prelude hiding (writeFile)

writeCss :: FilePath -> IO ()
writeCss path = writeFile path (render mainStylesheet)

-- | List of all Css Classes defined in Language.Ltml.HTML.CSS.Classes
cssClasses :: [Css]
cssClasses = map classStyle [minBound .. maxBound]

mainStylesheet :: Css
mainStylesheet = do
    body ? do
        fontFamily ["Arial"] [sansSerif]

    mconcat cssClasses
