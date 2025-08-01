{-# LANGUAGE FlexibleContexts #-}
module Language.Ltml.HTML.Util (intToLower, intToCapital, whenJust, convertNewLine, (<#>)) where

import Data.Char (chr)
import Lucid
import Language.Ltml.HTML.CSS.Classes as Class

-- | Converts Int to corresponding lowercase letter in the alphabet.
--   If Int is (<= 0) or (>= 27), it returns "?"
intToLower :: Int -> String
intToLower = intToLetter 96

-- | Converts Int to corresponding capital letter in the alphabet.
--   If Int is (<= 0) or (>= 27), it returns "?"
intToCapital :: Int -> String
intToCapital = intToLetter 64

-- | Converts Int to corresponding ASCII Char with offset shift.
--   If n is (<= 0) or (>= 27), it returns "?"
intToLetter :: Int -> Int -> String
intToLetter shift n
    | n == 0 = "?"
    | n <= 26 = (: []) $ chr (n + shift)
    | otherwise = intToLetter shift (mod n 27 + 1)

-------------------------------------------------------------------------------

-- | If maybe value is Nothing returns (), else passes a into function
whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust ma fa = maybe (pure ()) fa ma

-------------------------------------------------------------------------------

-- | Replaces every '\n' with HTML <br> while maintaining toHtml input sanitization
convertNewLine :: String -> Html ()
convertNewLine [] = mempty
convertNewLine s =
    let (raw, newLine) = break (== '\n') s
        in case newLine of
        [] -> toHtml raw
        (_:next) -> toHtml raw <> br_ [] <> convertNewLine next

-------------------------------------------------------------------------------

-- | Constructs HTML element with given Class 
(<#>) :: ([Attributes] -> t) -> Class -> t
htmlFunc <#> cssClass = htmlFunc [class_ (Class.className cssClass)]