module Language.Ltml.Parser.Section
    ( sectionP
    )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Star (Star))
import Language.Lsd.AST.Type (unwrapNT)
import Language.Lsd.AST.Type.Section
    ( HeadingType (HeadingType)
    , SectionBodyType (..)
    , SectionType (SectionType)
    )
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Section
    ( Heading (Heading)
    , Section (Section)
    , SectionBody (..)
    )
import Language.Ltml.Parser (Parser, wrapParser)
import Language.Ltml.Parser.Common.Indent (nonIndented)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Footnote (FootnoteParser)
import Language.Ltml.Parser.Footnote.Combinators (manyWithFootnotesTillSucc)
import Language.Ltml.Parser.Keyword (keywordP)
import Language.Ltml.Parser.Paragraph (paragraphP)
import Language.Ltml.Parser.SimpleBlock (simpleBlockP)
import Language.Ltml.Parser.Text (mlHangingTextP)
import Text.Megaparsec (many)

sectionP :: SectionType -> Parser () -> FootnoteParser (Node Section)
sectionP (SectionType kw headingT fmt bodyT) succStartP = do
    (mLabel, heading) <- wrapParser $ nonIndented $ headingP kw headingT
    body <- nonIndented $ bodyP bodyT
    return $ Node mLabel $ Section fmt heading body
  where
    bodyP :: SectionBodyType -> FootnoteParser SectionBody
    bodyP (InnerSectionBodyType (Star t)) =
        InnerSectionBody <$> many (sectionP t' (toStartP t' <|> succStartP))
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

toStartP :: SectionType -> Parser ()
toStartP (SectionType kw _ _ _) = void $ keywordP kw

headingP :: Keyword -> HeadingType -> Parser (Maybe Label, Heading)
headingP kw (HeadingType fmt tt) =
    nLexeme $ fmap (Heading fmt) <$> mlHangingTextP kw tt
