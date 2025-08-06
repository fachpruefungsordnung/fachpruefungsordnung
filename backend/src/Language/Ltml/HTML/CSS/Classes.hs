{-# LANGUAGE OverloadedStrings #-}
-- Turn incomplete pattern matches into error, so that every defined Class has to have a style
-- This ensures that every class used in Lucid also has an entry in the css stylesheet
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

module Language.Ltml.HTML.CSS.Classes
    ( Class (..)
    , className
    , classStyle
    , enumLevel
    , enumIdentifier
    ) where

import Clay hiding (i)
import qualified Clay.Flexbox as Flexbox
import Data.Char (toLower)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Language.Ltml.HTML.CSS.CustomClay
import Language.Ltml.HTML.Util (intToLower)

-- TODO: Add vertical space between Enumitems

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
    | -- | Class for aligning and spacing an enum item inside an enumeration
      EnumItem
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
classStyle EnumItem = mempty
classStyle Underlined = toClassSelector Underlined ? textDecoration underline
classStyle FontRed = toClassSelector FontRed ? fontColor red
classStyle EnumNum =
    enumCounter
        (className EnumNum)
        (counterNum "item" <> stringCounter ". ")
classStyle EnumCharPar =
    enumCounter
        (className EnumCharPar)
        (counterChar "item" <> stringCounter ") ")
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

-- | Returns the enum item identifier based on its class and number
enumIdentifier :: Class -> Int -> String
enumIdentifier EnumNum n = show n
enumIdentifier EnumCharPar n = intToLower n ++ ")"
enumIdentifier EnumCharCharPar n = let charId = intToLower n in charId ++ charId ++ ")"
enumIdentifier _ _ = "x."

-- | Builds CSS class with specfied counter for ordered lists
enumCounter :: Text -> Counter -> Css
enumCounter enumClassName counterContent = do
    ol # byClass enumClassName ? do
        counterReset "item"
        marginLeft (em 0)
        paddingLeft (em 0)
        marginTop (em 0)
        marginBottom (em 0.5)

    -- TODO: fix to much vertical space if a paragraph ends with
    --       an ol (ol and paragraph bottom margins add up)

    -- This could fix it but if raw text follows the ol it does not work,
    -- unless the text is wrapped in a span
    -- toClassSelector ParagraphText |> ol # byClass enumClassName ? lastChild & do
    --     marginBottom (em 0)

    ol # byClass enumClassName |> li ? do
        counterIncrement "item"
        display grid
        gridTemplateColumns [ch 3, fr 1]
        gap (em 0.5)
        marginTop (em 0.5)

    ol # byClass enumClassName |> li ? before & do
        counter counterContent
        textAlign alignRight

-------------------------------------------------------------------------------
