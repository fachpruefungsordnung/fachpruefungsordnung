module Language.Ltml.HTML.Util (intToLower, intToCapital) where

import Data.Char (chr)

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
intToLetter shift n | n == 0 = "?"
                    | n <= 26 = (:[]) $ chr (n + shift)
                    | otherwise = "?"

-------------------------------------------------------------------------------
