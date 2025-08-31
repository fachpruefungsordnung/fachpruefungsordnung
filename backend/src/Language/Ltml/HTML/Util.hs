{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Language.Ltml.HTML.Util
    ( intToLower
    , intToCapital
    , whenJust
    , mapState
    , withModified
    , convertNewLine
    , mId_
    , mTextId_
    , getNextRawTextTree
    , isSuper
    ) where

import Control.Monad.State (MonadState, gets, modify)
import Data.Char (chr)
import Data.Text (Text)
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Section (SectionBody (InnerSectionBody))
import Language.Ltml.AST.Text (TextTree (..))
import Language.Ltml.HTML.Common (GlobalState, HtmlReaderState)
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

-- | Saves state field and modifies it with a new temporary value for the span
--   of the given monadic action (like local); restores old state field afterwards
withModified
    :: (MonadState GlobalState m)
    => (GlobalState -> a)
    -> (GlobalState -> a -> GlobalState)
    -> a
    -> m b
    -> m b
withModified getter setter newValue action = do
    saved <- gets getter
    modify (\s -> setter s newValue)
    res <- action
    modify (\s -> setter s saved)
    return res

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

-- | Adds Text as id, if it exists
mTextId_ :: Maybe Text -> Attributes
mTextId_ Nothing = mempty
mTextId_ (Just text) = id_ text

-------------------------------------------------------------------------------

-- | Splits list into raw text part until next enumeration (raw is everything except enums)
getNextRawTextTree
    :: [TextTree lbrk fnref style enum special]
    -> ( [TextTree lbrk fnref style enum special]
       , [TextTree lbrk fnref style enum special]
       )
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
