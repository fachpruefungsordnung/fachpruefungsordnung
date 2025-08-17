module Language.Ltml.Parser.Table
    ( tableP
    )
where

import Language.Lsd.AST.Type.Table (TableType (TableType))
import Language.Ltml.AST.Table (Table (Table))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme1)
import Language.Ltml.Parser.Keyword (keywordP)

tableP :: TableType -> Parser Table
tableP (TableType kw) = Table <$ nLexeme1 (keywordP kw)
