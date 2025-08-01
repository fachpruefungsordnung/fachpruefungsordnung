{-# LANGUAGE OverloadedStrings #-}
-- Turn incomplete pattern matches into error, so that every defined Class has to have a style
-- This ensures that every class used in Lucid also has an entry in the css stylesheet
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

module Language.Ltml.HTML.CSS.Classes (Class (..), className, classStyle, enumLevel) where

import Clay
import qualified Clay.Flexbox as Flexbox
import Data.Char (toLower)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Language.Ltml.HTML.CSS.CustomClay

data Class
    = -- | Class for spacing and alignment of and inside of a section
      Section
    | -- | Class for spacing and alignment of a heading (h4 / h5)
      Heading
    | -- | Class for spacing and alignment of a paragraph div
      Paragraph
    | -- | Class for aligning a paragraph id div inside of a paragraph div
      ParagraphID
    | -- | Class for aligning the text inside a paragraph
      ParagraphText
    | -- | Underlining basic text
      Underlined
    | -- | Font color red
      FontRed
    | -- | Enum with 1., 2., 3., ...
      EnumNum
    | -- | Enum with a), b), c), ...
      EnumCharPar
    | -- | Enum with aa), bb), cc), ...
      EnumCharCharPar
    | -- | Enum for errors
      EnumFail
    deriving (Show, Eq, Enum, Bounded)

-- | maps Class to its css style definition
classStyle :: Class -> Css
classStyle Section =
    toClassSelector Section ? do
        display block
        marginTop (em 2)
classStyle Heading =
    toClassSelector Heading ? do
        textAlign center
        fontWeight bold
        marginBottom (em 0)
classStyle Paragraph =
    toClassSelector Paragraph ? do
        display flex
        marginTop (em 1)
        marginBottom (em 1)
classStyle ParagraphID = toClassSelector ParagraphID ? Flexbox.flex 0 0 (em 2)
classStyle ParagraphText = toClassSelector ParagraphText ? textAlign justify
classStyle Underlined = toClassSelector Underlined ? textDecoration underline
classStyle FontRed = toClassSelector FontRed ? fontColor red
classStyle EnumNum =
    enumCounter
        (className EnumNum)
        (counterNum "item" <> stringCounter ". ")
classStyle EnumCharPar =
    enumCounter (className EnumCharPar) (counterChar "item" <> stringCounter ") ")
classStyle EnumCharCharPar =
    enumCounter
        (className EnumCharCharPar)
        (counterChar "item" <> counterChar "item" <> stringCounter ") ")
classStyle EnumFail = enumCounter (className EnumFail) (stringCounter "x. ")

-- | Returns the html class name of given Class
className :: Class -> Text
className cssClass = case show cssClass of
    [] -> error "CSS Class has \"\" as show instance!"
    (c : cs) -> pack $ toLower c : cs

-- | converts Class to Clay Selector and adds "." infront for css selection
toClassSelector :: Class -> Selector
toClassSelector c = fromString ("." ++ unpack (className c))

-------------------------------------------------------------------------------

-- | Example Enumertion Levels for an FPO
enumLevel :: Int -> Class
enumLevel i = case i of
    0 -> EnumNum
    1 -> EnumCharPar
    2 -> EnumCharCharPar
    _ -> EnumFail -- dont throw error but place placeholder symbol

-- | Builds CSS class with specfied counter for ordered lists
enumCounter :: Text -> Counter -> Css
enumCounter enumClassName counterContent = do
    ol # byClass enumClassName ? do
        counterReset "item"

    ol # byClass enumClassName |> li ? do
        display block

    ol # byClass enumClassName |> li ? before & do
        counter counterContent
        counterIncrement "item"
