module Language.Ltml.Parser.Document
    ( documentHeadingP
    )
where

import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.Type.Document
    ( DocumentHeadingType (DocumentHeadingType)
    )
import Language.Ltml.AST.Document
    ( DocumentHeading (DocumentHeading)
    )
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Text (HangingTextP, hangingTextP')

documentHeadingP
    :: (HangingTextP f)
    => Keyword
    -> DocumentHeadingType
    -> Parser (f DocumentHeading)
documentHeadingP kw (DocumentHeadingType tt) =
    nLexeme $ fmap DocumentHeading <$> hangingTextP' kw tt
