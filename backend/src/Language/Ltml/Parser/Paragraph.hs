module Language.Ltml.Parser.Paragraph
    ( paragraphP
    )
where

import Control.Applicative (optional)
import Control.Monad.State (evalStateT)
import Language.Lsd.AST.Type.Paragraph (ParagraphType (ParagraphType))
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Paragraph (Paragraph (Paragraph))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Indent (nonIndented)
import Language.Ltml.Parser.Common.Lexeme (nLexeme1)
import Language.Ltml.Parser.Label (labelingP)
import Language.Ltml.Parser.Text (ParagraphParser, textForestP)
import Text.Megaparsec (try)

paragraphP :: ParagraphType -> Parser (Node Paragraph)
paragraphP (ParagraphType fmt tt) =
    Node
        -- TODO: Avoid `try`.
        <$> nonIndented (optional . try $ nLexeme1 labelingP)
        <*> nonIndented (evalStateT bodyP True)
  where
    bodyP :: ParagraphParser Paragraph
    bodyP = Paragraph fmt <$> textForestP tt
