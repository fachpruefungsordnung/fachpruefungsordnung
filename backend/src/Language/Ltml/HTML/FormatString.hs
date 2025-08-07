module Language.Ltml.HTML.FormatString (sectionFormat, headingFormat, paragraphFormat) where

import Control.Monad.Writer
import Language.Lsd.AST.Format
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (..))
import Language.Lsd.AST.Type.Section (SectionFormat (..))
import Language.Ltml.HTML.Util
import Lucid (Html, ToHtml (toHtml))
import Prelude hiding (id)

-- | Builds Heading text based on given FormatString, id and text
headingFormat :: HeadingFormat -> Html () -> Html () -> Html ()
headingFormat (FormatString []) _ _ = mempty
headingFormat (FormatString (a : as)) id text =
    case a of
        -- \| replaces '\n' with <br>
        StringAtom s -> convertNewLine s
        PlaceholderAtom IdentifierPlaceholder -> id
        PlaceholderAtom HeadingTextPlaceholder -> toHtml text
        <> headingFormat (FormatString as) id text

sectionFormat :: SectionFormat -> Int -> Html ()
sectionFormat (SectionFormat fs) i = fst $ runWriter $ identifierFormat fs i

paragraphFormat :: ParagraphFormat -> Int -> (Html (), Maybe (Html ()))
paragraphFormat (ParagraphFormat fs) i = runWriter $ identifierFormat fs i

-- | Builds id text based on given FormatString and id.
--   Also returns the (last seen) raw ID for references to this paragraph via a Writer monad.
--   We do this because when we reference a paragraph we have to know how its raw id should be displayed.
identifierFormat
    :: IdentifierFormat -> Int -> Writer (Maybe (Html ())) (Html ())
identifierFormat (FormatString []) _ = return mempty
identifierFormat (FormatString (a : as)) id = do
    b <- case a of
        -- \| replaces '\n' with <br>
        StringAtom s -> return $ convertNewLine s
        PlaceholderAtom Arabic -> tellAndReturnJust $ toHtml $ show id
        -- \| convert paragraphID to single letter string
        PlaceholderAtom AlphabeticLower -> tellAndReturnJust $ toHtml $ intToLower id
        PlaceholderAtom AlphabeticUpper -> tellAndReturnJust $ toHtml $ intToCapital id
    bs <- identifierFormat (FormatString as) id
    return $ b <> bs

tellAndReturnJust :: (Semigroup a) => a -> Writer (Maybe a) a
tellAndReturnJust a = do
    tell $ Just a
    return a
