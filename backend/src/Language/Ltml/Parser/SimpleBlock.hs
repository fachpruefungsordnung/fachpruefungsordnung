module Language.Ltml.Parser.SimpleBlock
    ( simpleBlockP
    )
where

import Control.Applicative ((<|>))
import Language.Lsd.AST.Type (unwrapNT)
import Language.Lsd.AST.Type.SimpleBlock (SimpleBlockType (SimpleBlockType))
import Language.Ltml.AST.SimpleBlock (SimpleBlock (..))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Module (moduleBlockP)
import Language.Ltml.Parser.SimpleParagraph (simpleParagraphP)
import Language.Ltml.Parser.Table (tableP)

simpleBlockP :: SimpleBlockType -> Parser SimpleBlock
simpleBlockP (SimpleBlockType parT tableT moduleBT) =
    -- Parsing a paragraph must be attempted last, for it does not have a
    -- keyword; i.e., generally treats a keyword as plain text.
    TableBlock <$> tableP (unwrapNT tableT)
        <|> TableBlock <$> moduleBlockP (unwrapNT moduleBT)
        <|> SimpleParagraphBlock <$> simpleParagraphP (unwrapNT parT)
