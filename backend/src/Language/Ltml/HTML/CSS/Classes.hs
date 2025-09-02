{-# LANGUAGE OverloadedStrings #-}
-- Turn incomplete pattern matches into error, so that every defined Class has to have a style
-- This ensures that every class used in Lucid also has an entry in the css stylesheet
{-# OPTIONS_GHC -Wincomplete-patterns -Werror=incomplete-patterns #-}

module Language.Ltml.HTML.CSS.Classes
    ( -- * CSS Class Defintions
      Class (..)
    , className
    , classStyle

      -- * Enumeration Counter
    , enumCounter

      -- * LTML AST to CSS Class Mapping
    , ToCssClass (..)
    , toCssClasses
    ) where

import Clay hiding (i, map, size)
import qualified Clay.Flexbox as Flexbox
import Data.Char (toLower)
import Data.String (fromString)
import Data.Text (Text, pack, unpack)
import qualified Data.Typography as Ltml
import Data.Void (Void, absurd)
import Language.Ltml.HTML.CSS.CustomClay

data Class
    = -- | Class for styling that should be applied to the whole document (HTML body)
      Body
    | -- | Class for spacing and alignment of and inside of an appendix section
      AppendixSection
    | -- | Class for styling and aligning document title <h1>
      DocumentTitle
    | -- | Class for spacing and alignment of and inside of a super-section
      SuperSection
    | -- | Class for spacing and alignment of and inside of a section
      Section
    | -- | Class for spacing and alignment of a heading <h2>
      Heading
    | -- | Class for spacing and alignment of a paragraph div
      Paragraph
    | -- | Class for aligning a paragraph id div inside of a paragraph div
      ParagraphID
    | -- | Text container which spaces text with elements in it (e.g. enumerations)
      TextContainer
    | -- | General class for all enumerations
      Enumeration
    | -- | Class for spacing and alignment of multiple footnotes
      FootnoteContainer
    | -- | Class for spacing and alignment of a single footnote
      Footnote
    | -- | Class for <sup> element when rendering footnotes (not refs)
      FootnoteID
    | -- | Class for containers which left align text
      LeftAligned
    | -- | Class for containers which centers text
      Centered
    | -- | Class for containers which right align text
      RightAligned
    | -- | Class for containers with a smaller font size
      SmallFontSize
    | -- | Class for containers with a standard/medium font size
      MediumFontSize
    | -- | Class for containers with a larger font size
      LargeFontSize
    | -- | Bold inlined basic text
      Bold
    | -- | Italic inlined basic text
      Italic
    | -- | Underlined inlined basic text
      Underlined
    | -- | Class which inlines a red bold error text
      InlineError
    deriving (Show, Eq, Enum, Bounded)

-- | Maps Class to its css style definition
classStyle :: Class -> Css
classStyle Body =
    toClassSelector Body ? do
        fontFamily ["Arial"] [sansSerif]
        lineHeight (unitless 1.5)
        marginTop (em 2)
        marginLeft (em 2)
        marginRight (em 2)
        -- \| make Document scrollable past its end
        paddingBottom (em 10)
classStyle AppendixSection =
    toClassSelector AppendixSection ? do
        marginTop (em 10)
        display flex
        flexDirection column
        -- \| gap between documents inside an appendix section
        gap (em 10)
classStyle DocumentTitle =
    toClassSelector DocumentTitle ? do
        marginTop (em 0)
        marginBottom (em 0)
        fontSize (em 1.5)
classStyle SuperSection =
    toClassSelector SuperSection ? do
        display flex
        flexDirection column
        -- \| gap between sections
        gap (em 3)
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
        --       or Document even. Maybe it would be best to track the largest paragraph id and then
        --       scale all paragraphs to fit the largest one (let i = length maxid in flex 0 0 (em i))
        -- Edit: Right now i dont like this idea; maybe just leave it as is
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
classStyle FootnoteContainer =
    toClassSelector FootnoteContainer ? do
        marginTop (em 1)
classStyle Footnote =
    toClassSelector Footnote ? do
        display flex
        -- \| Gap between footnote id and footnote text
        gap (em 0.5)
classStyle FootnoteID =
    toClassSelector FootnoteID ? do
        userSelect none
classStyle LeftAligned = toClassSelector LeftAligned ? textAlign alignLeft
classStyle Centered = toClassSelector Centered ? textAlign center
classStyle RightAligned = toClassSelector RightAligned ? textAlign alignLeft
classStyle SmallFontSize = toClassSelector SmallFontSize ? fontSize (em 0.75)
classStyle MediumFontSize = toClassSelector MediumFontSize ? fontSize (em 1)
classStyle LargeFontSize = toClassSelector LargeFontSize ? fontSize (em 1.25)
classStyle Bold =
    toClassSelector Bold ? do
        fontWeight bold
classStyle Italic =
    toClassSelector Italic ? do
        fontStyle italic
classStyle Underlined = do
    toClassSelector Underlined ? do
        textDecoration underline
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
                -- \| Enum indentation
                paddingLeft (em 1)
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

-- | Converts Class to Clay Selector and adds "." infront for css selection
toClassSelector :: Class -> Selector
toClassSelector c = fromString ("." ++ unpack (className c))

-------------------------------------------------------------------------------

-- | Builds CSS class with specfied counter for ordered list items
enumCounter :: Text -> Counter -> Css
enumCounter enumClassName counterContent = do
    ol # byClass enumClassName |> li ? before & do
        counter counterContent
        textAlign alignRight

-------------------------------------------------------------------------------

class ToCssClass a where
    toCssClass :: a -> Class

instance ToCssClass Ltml.TextAlignment where
    toCssClass align = case align of
        Ltml.LeftAligned -> LeftAligned
        Ltml.Centered -> Centered
        Ltml.RightAligned -> RightAligned

instance ToCssClass Ltml.FontSize where
    toCssClass size = case size of
        Ltml.SmallFontSize -> SmallFontSize
        Ltml.MediumFontSize -> MediumFontSize
        Ltml.LargeFontSize -> LargeFontSize

instance ToCssClass Ltml.FontStyle where
    toCssClass size = case size of
        Ltml.Bold -> Bold
        Ltml.Italics -> Italic
        Ltml.Underlined -> Underlined

-- | ToCssClass instance that can never be called, because there are
--   no values of type Void
instance ToCssClass Void where
    toCssClass = absurd

-- | Converts Typography into list of CSS classes that implement each feature
toCssClasses :: Ltml.Typography -> [Class]
toCssClasses (Ltml.Typography align size styles) =
    let styleClasses = map toCssClass styles
     in toCssClass align : toCssClass size : styleClasses
