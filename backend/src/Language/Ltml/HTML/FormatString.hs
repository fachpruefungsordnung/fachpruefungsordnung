module Language.Ltml.HTML.FormatString (sectionFormat, headingFormat, paragraphFormat) where

import Language.Lsd.AST.Format
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (..))
import Language.Lsd.AST.Type.Section (SectionFormat (..))
import Language.Ltml.HTML.Util (intToCapital, intToLower)
import Lucid (Html, ToHtml (toHtml))
import Prelude hiding (id)

-- | Builds Heading text based on given FormatString, id and text
headingFormat :: HeadingFormat -> Html () -> Html () -> Html ()
headingFormat (FormatString []) _ _ = mempty
headingFormat (FormatString (a : as)) id text =
    case a of
        StringAtom s -> toHtml s
        PlaceholderAtom IdentifierPlaceholder -> id
        PlaceholderAtom HeadingTextPlaceholder -> toHtml text
        <> headingFormat (FormatString as) id text

sectionFormat :: SectionFormat -> Int -> Html ()
sectionFormat (SectionFormat fs) = identifierFormat fs

paragraphFormat :: ParagraphFormat -> Int -> Html ()
paragraphFormat (ParagraphFormat fs) = identifierFormat fs

-- | Builds id text based on given FormatString and id
identifierFormat :: IdentifierFormat -> Int -> Html ()
identifierFormat (FormatString []) _ = mempty
identifierFormat (FormatString (a : as)) id =
    case a of
        StringAtom s -> toHtml s
        PlaceholderAtom Arabic -> toHtml $ show id
        -- \| convert paragraphID to single letter string
        PlaceholderAtom AlphabeticLower -> toHtml $ intToLower id
        PlaceholderAtom AlphabeticUpper -> toHtml $ intToCapital id
        <> identifierFormat (FormatString as) id
