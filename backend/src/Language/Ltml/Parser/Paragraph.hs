module Language.Ltml.Parser.Paragraph
    ( paragraphP
    )
where

import Language.Lsd.AST.Type.Paragraph (ParagraphType (..))
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.AST.Paragraph (Paragraph (..))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Text (textForestP)

-- TODO: Parse node label.
paragraphP :: ParagraphType -> Parser (Node Paragraph)
paragraphP (ParagraphType fmt tt) =
    Node Nothing . Paragraph fmt <$> textForestP tt
