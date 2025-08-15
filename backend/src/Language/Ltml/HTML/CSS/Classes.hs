{-# LANGUAGE OverloadedStrings #-}
-- Turn incomplete pattern matches into error, so that every defined Class has to have a style
-- This ensures that every class used in Lucid also has an entry in the css stylesheet
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

module Language.Ltml.HTML.CSS.Classes
    ( Class (..)
    , className
    , classStyle
    , enumCounter
    ) where

import Clay hiding (i)
import qualified Clay.Flexbox as Flexbox
import Data.Char (toLower)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import Language.Ltml.HTML.CSS.CustomClay

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
    | -- | General class for all enumerations
      Enumeration
    | -- | Underlined inlined basic text
      Underlined
    | -- | Bold inlined basic text
      Bold
    | -- | Italic inlined basic text
      Italic
    | -- | Class which inlines a red bold error text
      InlineError
    deriving (Show, Eq, Enum, Bounded)

-- | maps Class to its css style definition
classStyle :: Class -> Css
classStyle Document =
    toClassSelector Document ? do
        fontFamily ["Arial"] [sansSerif]
        lineHeight (unitless 1.5)
        marginTop (em 2)
        marginLeft (em 2)
        marginRight (em 2)
        -- \| make Document scrollable past its end
        paddingBottom (em 10)
classStyle Section =
    toClassSelector Section ? do
        display flex
        flexDirection column
        -- \| gap between paragraphs
        gap (em 1)
classStyle Heading =
    toClassSelector Heading ? do
        textAlign center
        fontWeight bold
        marginTop (em 0)
        marginBottom (em 0)
        fontSize (em 1)
classStyle Paragraph =
    toClassSelector Paragraph ? do
        display flex
classStyle ParagraphID =
    toClassSelector ParagraphID ? do
        Flexbox.flex 0 0 auto
        -- TODO: if some paragraph ids are larger than others (e.g. (1) and (42))
        --       the larger paragraph will have a bigger indentation than the smaller one
        --       Might be needed to make paragraphs <li> and section <ol> and apply a
        --       custom css counter to each paragraph <li>
        --       Actually the indentation of paragraphs should be the same across the whole Section
        --       or Document even. Maybe it would be best to track the largest paragrapg id and then
        --       scale all paragraphs to fit the largest one (let i = length maxid in flex 0 0 (em i))
        -- \| Gap between paragraph id and text
        paddingRight (em 0.75)
        userSelect none
classStyle TextContainer =
    toClassSelector TextContainer ? do
        display flex
        flexDirection column
        -- \| gap between text and enumerations
        gap (em 0.5)
        textAlign justify
classStyle Underlined = do
    toClassSelector Underlined ? do
        textDecoration underline
classStyle Bold =
    toClassSelector Bold ? do
        fontWeight bold
classStyle Italic =
    toClassSelector Italic ? do
        fontStyle italic
classStyle InlineError =
    toClassSelector InlineError ? do
        fontColor red
        fontWeight bold
classStyle Enumeration =
    let enumClassName = className Enumeration
     in do
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
                gridTemplateColumns [auto, fr 1]
                -- \| gap between enum item id and enum text
                gap (em 0.55)
                marginTop (em 0)
                marginBottom (em 0)

-- | Returns the html class name of given Class
className :: Class -> Text
className cssClass = case show cssClass of
    [] -> error "CSS Class has \"\" as show instance!"
    (c : cs) -> pack $ toLower c : cs

-- | converts Class to Clay Selector and adds "." infront for css selection
toClassSelector :: Class -> Selector
toClassSelector c = fromString ("." ++ unpack (className c))

-------------------------------------------------------------------------------

-- | Returns the enum item identifier based on its class and number
-- enumIdentifier :: Class -> Int -> String

-- | Builds CSS class with specfied counter for ordered list items
enumCounter :: Text -> Counter -> Css
enumCounter enumClassName counterContent = do
    ol # byClass enumClassName |> li ? before & do
        counter counterContent
        textAlign alignRight

-------------------------------------------------------------------------------
