module Language.Ltml.Parser.Section
    ( sectionP
    )
where

import Control.Applicative ((<|>))
import Control.Monad (void)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Star (Star))
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
import Language.Ltml.Parser (Parser, nonIndented)
import Language.Ltml.Parser.Common.Combinators (manyTillSucc)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Keyword (keywordP)
import Language.Ltml.Parser.Paragraph (paragraphP)
import Language.Ltml.Parser.SimpleBlock (simpleBlockP)
import Language.Ltml.Parser.Text (hangingTextP')
import Text.Megaparsec (many)

sectionP :: SectionType -> Parser () -> Parser (Node Section)
sectionP (SectionType kw headingT fmt bodyT) succStartP = do
    (mLabel, heading) <- nonIndented $ headingP kw headingT
    body <- nonIndented $ bodyP bodyT
    return $ Node mLabel $ Section fmt heading body
  where
    bodyP :: SectionBodyType -> Parser SectionBody
    bodyP (InnerSectionBodyType (Star t)) =
        InnerSectionBody <$> many (sectionP t (toStartP t <|> succStartP))
    bodyP (LeafSectionBodyType (Star t)) =
        LeafSectionBody <$> manyTillSucc (nLexeme $ paragraphP t) succStartP
    bodyP (SimpleLeafSectionBodyType (Star t)) =
        SimpleLeafSectionBody
            <$> manyTillSucc (nLexeme $ simpleBlockP t) succStartP

toStartP :: SectionType -> Parser ()
toStartP (SectionType kw _ _ _) = void $ keywordP kw

headingP :: Keyword -> HeadingType -> Parser (Maybe Label, Heading)
headingP kw (HeadingType fmt tt) =
    nLexeme $ fmap (Heading fmt) <$> hangingTextP' kw tt
