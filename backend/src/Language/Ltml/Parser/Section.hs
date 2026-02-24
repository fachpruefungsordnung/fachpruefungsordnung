module Language.Ltml.Parser.Section
    ( sectionP
    , sectionBodyP
    , headingP
    )
where

import Control.Applicative ((<|>))
import Control.Functor.Utils (traverseF)
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty (..))
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Star (Star))
import Language.Lsd.AST.Type (unwrapNT)
import Language.Lsd.AST.Type.Section
    ( FormattedSectionType
    , HeadingType (HeadingType)
    , SectionBodyType (..)
    , SectionFormatted (SectionFormatted)
    , SectionType (SectionType)
    )
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Section
    ( FormattedSection
    , Heading (Heading)
    , Section (Section)
    , SectionBody (..)
    )
import Language.Ltml.Common (Flagged (Flagged))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Footnote (FootnoteParser)
import Language.Ltml.Parser.Footnote.Combinators
    ( manyWithFootnotesTillSucc
    , withSucceedingFootnotes
    )
import Language.Ltml.Parser.Keyword (keywordP)
import Language.Ltml.Parser.Paragraph (paragraphP)
import Language.Ltml.Parser.SimpleBlock (simpleBlockP)
import Language.Ltml.Parser.Text (HangingTextP, hangingTextP')
import Text.Megaparsec (MonadParsec (try), choice, many)

sectionP :: SectionType -> Parser () -> FootnoteParser (Node Section)
sectionP (SectionType kw headingT bodyT) succStartP = do
    (mLabel, heading) <- headingP kw headingT
    body <- sectionBodyP bodyT succStartP
    return $ Node mLabel $ Section (Right heading) body

sectionP'
    :: FormattedSectionType -> Parser () -> FootnoteParser FormattedSection
sectionP' (t :| ts) succStartP = choice (map (try . singleSectionP' succStartP) (ts ++ [t]))
  where
    singleSectionP'
        :: Parser ()
        -> SectionFormatted SectionType
        -> FootnoteParser FormattedSection
    singleSectionP' succStartP' = traverseF (\st -> Right <$> sectionP st succStartP')

sectionBodyP :: SectionBodyType -> Parser () -> FootnoteParser SectionBody
sectionBodyP t0 succStartP = bodyP t0
  where
    bodyP :: SectionBodyType -> FootnoteParser SectionBody
    bodyP (InnerSectionBodyType (Star t)) =
        InnerSectionBody
            <$> many
                ( Flagged False
                    <$> sectionP' t' (toStartP t' <|> succStartP)
                )
      where
        t' = unwrapNT t
    bodyP (LeafSectionBodyType (Star t)) =
        LeafSectionBody
            <$> manyWithFootnotesTillSucc (paragraphP $ unwrapNT t) succStartP
    bodyP (SimpleLeafSectionBodyType (Star t)) =
        SimpleLeafSectionBody
            <$> manyWithFootnotesTillSucc
                (simpleBlockP $ unwrapNT t)
                succStartP

toStartP :: FormattedSectionType -> Parser ()
toStartP (t :| ts) = choice (map (try . toStartP') (ts ++ [t]))
  where
    toStartP' :: SectionFormatted SectionType -> Parser ()
    toStartP' (SectionFormatted _ (SectionType kw _ _)) = void $ keywordP kw

headingP
    :: (HangingTextP f)
    => Keyword
    -> HeadingType
    -> FootnoteParser (f Heading)
headingP kw (HeadingType fmt tt) =
    withSucceedingFootnotes $
        nLexeme $
            fmap (Heading fmt) <$> hangingTextP' kw tt
