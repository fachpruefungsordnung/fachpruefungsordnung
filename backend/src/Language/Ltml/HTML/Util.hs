{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Util
    ( intToLower
    , intToCapital
    , whenJust
    , mapState
    , convertNewLine
    , mId_
    , anchorLink
    , getNextRawTextTree
    , isSuper
    ) where

import Data.Char (chr)
import Data.Text (cons)
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Section (SectionBody (InnerSectionBody))
import Language.Ltml.AST.Text (TextTree (..))
import Lucid

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

-- | Applies functions to every item in the list and
--   chains those calls together by propagating the state s from
--   left to right; the final state is dropped
mapState :: (Monad m) => (s -> a -> m s) -> s -> [a] -> m ()
mapState _ _ [] = pure ()
mapState f s (a : as) = do
    s' <- f s a
    mapState f s' as

-------------------------------------------------------------------------------

-- | Replaces every '\n' with HTML <br> while maintaining toHtml input sanitization
convertNewLine :: String -> Html ()
convertNewLine [] = mempty
convertNewLine s =
    let (raw, newLine) = break (== '\n') s
     in case newLine of
            [] -> toHtml raw
            (_ : next) -> toHtml raw <> br_ [] <> convertNewLine next

-------------------------------------------------------------------------------

-- | Adds Label as id, if it exists
mId_ :: Maybe Label -> Attributes
mId_ Nothing = mempty
mId_ (Just label) = id_ $ unLabel label

-- | Converts Label into <a href = "#<label>"> for jumping to a HTML id
anchorLink :: Label -> Html () -> Html ()
anchorLink label = a_ [href_ (cons '#' $ unLabel label)]

-------------------------------------------------------------------------------

-- | Splits list into raw text part until next enumeration (raw is everything except enums)
getNextRawTextTree
    :: [TextTree style enum special]
    -> ([TextTree style enum special], [TextTree style enum special])
getNextRawTextTree =
    break
        ( \case
            Enum _ -> True
            _ -> False
        )

-- | Is given section a super-section? (has sections as children)
isSuper :: SectionBody -> Bool
isSuper (InnerSectionBody _) = True
isSuper _ = False
