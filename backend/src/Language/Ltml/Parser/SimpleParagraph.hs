module Language.Ltml.Parser.SimpleParagraph
    ( simpleParagraphP
    )
where

import Language.Lsd.AST.Type.SimpleParagraph
    ( SimpleParagraphType (SimpleParagraphType)
    )
import Language.Ltml.AST.SimpleParagraph (SimpleParagraph (SimpleParagraph))
import Language.Ltml.Parser (Parser, nonIndented)
import Language.Ltml.Parser.Text (textForestP)

simpleParagraphP :: SimpleParagraphType -> Parser SimpleParagraph
simpleParagraphP (SimpleParagraphType fmt tt) =
    SimpleParagraph fmt <$> nonIndented (textForestP tt)
