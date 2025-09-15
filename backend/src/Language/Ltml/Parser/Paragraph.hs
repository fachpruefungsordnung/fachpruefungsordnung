module Language.Ltml.Parser.Paragraph
    ( paragraphP
    )
where

import Control.Applicative (optional, (<|>))
import Control.Monad.State (evalStateT)
import Data.Maybe (isJust)
import Language.Lsd.AST.Type.Paragraph (ParagraphType (ParagraphType))
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Paragraph (Paragraph (Paragraph))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme1)
import Language.Ltml.Parser.Label (labelingP)
import Language.Ltml.Parser.Text (textForestP)
import Text.Megaparsec (try)

paragraphP :: ParagraphType -> Parser (Node Paragraph)
paragraphP (ParagraphType fmt tt) = do
    -- Note:
    --  * Backtracking due to `try` is limited to
    --    `labelingP <* sp <* char '\n'`, and this is typically cheap.
    --    - CONSIDER: Avoid `try`.
    mLabel <- optional (try $ nLexeme1 labelingP)
    body <-
        if isJust mLabel
            then bodyP <|> pure []
            else bodyP
    return $ Node mLabel (Paragraph fmt body)
  where
    bodyP = evalStateT (textForestP tt) True
