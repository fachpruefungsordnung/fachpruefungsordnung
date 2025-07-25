module Language.Ltml.Parser.Paragraph
    ( paragraphP
    )
where

import Control.Applicative (optional)
import Control.Monad.State (evalStateT)
import Language.Lsd.AST.Type.Paragraph (ParagraphType (ParagraphType))
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Paragraph (Paragraph (Paragraph))
import Language.Ltml.Parser (Parser, nonIndented, sp)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Label (labelingP)
import Language.Ltml.Parser.Text (textForestP)
import Text.Megaparsec (notFollowedBy, try)
import Text.Megaparsec.Char (char)

paragraphP :: ParagraphType -> Parser () -> Parser (Node Paragraph)
paragraphP (ParagraphType fmt tt) succStartP = do
    -- Do not parse what should be a new section.
    notFollowedBy $ try succStartP

    -- TODO: Avoid `try`.
    flip evalStateT True $ do
        label <-
            nonIndented . optional . try $
                nLexeme (labelingP <* sp <* char '\n')
        Node label . Paragraph fmt <$> nonIndented (textForestP tt)
