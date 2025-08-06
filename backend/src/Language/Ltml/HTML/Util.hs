{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Util
    ( intToLower
    , intToCapital
    , whenJust
    , mapState
    , convertNewLine
    , addHtmlHeader
    , addInlineCssHeader
    , mId_
    , anchorLink
    ) where

import Clay (Css, render)
import Data.Char (chr)
import Data.Text (cons, pack)
import Data.Text.Lazy (toStrict)
import Language.Ltml.AST.Label (Label (..))
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

-- | Adds html, head and body tags onto given html and
--   sets title and css path
addHtmlHeader :: String -> FilePath -> Html () -> Html ()
addHtmlHeader title cssPath html = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        link_ [rel_ "stylesheet", href_ (pack cssPath)]
    body_ html

-- | Adds html, head and body tags onto given html and
--   sets title, renders and inlines given css
addInlineCssHeader :: String -> Css -> Html () -> Html ()
addInlineCssHeader title css html =
    doctypehtml_ $ do
        head_ $ do
            title_ (toHtml title)
            style_ (toStrict $ render css)
        body_ html

-- | Adds Label as id, if it exists
mId_ :: Maybe Label -> Attributes
mId_ Nothing = mempty
mId_ (Just label) = id_ $ unLabel label

-- | Converts Label into <a href = "#<label>"> for HTML jumping
anchorLink :: Label -> Html () -> Html ()
anchorLink label = a_ [href_ (cons '#' $ unLabel label)]
