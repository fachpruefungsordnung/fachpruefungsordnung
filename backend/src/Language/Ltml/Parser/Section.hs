module Language.Ltml.Parser.Section
    ( sectionP
    )
where

import Data.Bitraversable (bitraverse)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (SimpleRegex)
import Language.Lsd.AST.Type.Paragraph (ParagraphType)
import Language.Lsd.AST.Type.Section
    ( HeadingType (HeadingType)
    , SectionType (SectionType)
    )
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Paragraph (Paragraph)
import Language.Ltml.AST.Section
    ( Heading (Heading)
    , Section (Section)
    )
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Common.SimpleRegex (simpleRegexP)
import Language.Ltml.Parser.Paragraph (paragraphP)
import Language.Ltml.Parser.Text (hangingTextP')
import Text.Megaparsec (many)

sectionP :: SectionType -> Parser (Node Section)
sectionP (SectionType kw headingT fmt childrenT) = do
    (mLabel, heading) <- headingP kw headingT
    Node mLabel . Section fmt heading <$> bitraverse parsP secsP childrenT
  where
    parsP :: ParagraphType -> Parser [Node Paragraph]
    parsP t = many $ nLexeme $ paragraphP t

    secsP :: SimpleRegex SectionType -> Parser [Node Section]
    secsP = simpleRegexP sectionP

headingP :: Keyword -> HeadingType -> Parser (Maybe Label, Heading)
headingP kw (HeadingType fmt tt) =
    nLexeme $ fmap (Heading fmt) <$> hangingTextP' kw tt
