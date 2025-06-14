module Language.Ltml.Parser.Section
    ( sectionP
    )
where

import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Type.Section
    ( HeadingType (HeadingType)
    , SectionChildType (SectionChildParagraphType, SectionChildSectionType)
    , SectionType (SectionType)
    )
import Language.Ltml.AST.Label (Label)
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Section
    ( Heading (Heading)
    , Section (Section)
    , SectionChild (SectionChildParagraph, SectionChildSection)
    )
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Common.SimpleRegex (simpleRegexP)
import Language.Ltml.Parser.Paragraph (paragraphP)
import Language.Ltml.Parser.Text (hangingTextP')

sectionP :: SectionType -> Parser (Node Section)
sectionP (SectionType kw headingT fmt childrenTR) = do
    (mLabel, heading) <- headingP kw headingT
    Node mLabel . Section fmt heading <$> childrenP
  where
    childrenP :: Parser [SectionChild]
    childrenP = simpleRegexP sectionChildP childrenTR

headingP :: Keyword -> HeadingType -> Parser (Maybe Label, Heading)
headingP kw (HeadingType fmt tt) =
    nLexeme $ fmap (Heading fmt) <$> hangingTextP' kw tt

sectionChildP :: SectionChildType -> Parser SectionChild
sectionChildP (SectionChildSectionType t) =
    SectionChildSection <$> sectionP t
sectionChildP (SectionChildParagraphType t) =
    nLexeme $ SectionChildParagraph <$> paragraphP t
