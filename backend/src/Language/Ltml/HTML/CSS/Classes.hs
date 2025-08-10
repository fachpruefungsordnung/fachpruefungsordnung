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

data Class
    = -- | Class for styling that should be applied to the whole document (HTML body)
      Document
    | -- | Class for spacing and alignment of and inside of a section
      Section
    | -- | Class for spacing and alignment of a heading (h4 / h5)
      Heading
    | -- | Class for spacing and alignment of a paragraph div
      Paragraph
    | -- | Class for aligning a paragraph id div inside of a paragraph div
      ParagraphID
    | -- | Text container which spaces text with elements in it (e.g. enumerations)
      TextContainer
    | -- | Underlining basic text
      Underlined
    | -- | Class which inlines a red bold error text
      InlineError
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
classStyle Document =
    toClassSelector Document ? do
        fontFamily ["Arial"] [sansSerif]
        marginLeft (em 2)
        marginRight (em 2)
classStyle Section =
    toClassSelector Section ? do
        display flex
        flexDirection column
        -- \| gap between paragraphs
        gap (em 1)
        marginTop (em 2)
classStyle Heading =
    toClassSelector Heading ? do
        textAlign center
        fontWeight bold
        marginBottom (em 0)
classStyle Paragraph =
    toClassSelector Paragraph ? do
        display flex
classStyle ParagraphID = toClassSelector ParagraphID ? Flexbox.flex 0 0 (em 2)
classStyle TextContainer =
    toClassSelector TextContainer ? do
        display flex
        flexDirection column
        -- \| gap between text and enumerations
        gap (em 0.5)
        textAlign justify
classStyle Underlined = toClassSelector Underlined ? textDecoration underline
classStyle InlineError =
    toClassSelector InlineError ? do
        -- \| inlines content in flex environment
        display displayContents
        fontColor red
        fontWeight bold
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
        marginBottom (em 0)
        -- \| enums items are also spaced via flex environment
        display flex
        flexDirection column
        -- \| gap between two enum items
        gap (em 0.5)

    ol # byClass enumClassName |> li ? do
        counterIncrement "item"
        display grid
        gridTemplateColumns [ch 3, fr 1]
        -- \| gap between enum item id and enum text
        gap (em 0.5)
        marginTop (em 0)
        marginBottom (em 0)

    ol # byClass enumClassName |> li ? before & do
        counter counterContent
        textAlign alignRight

-------------------------------------------------------------------------------
