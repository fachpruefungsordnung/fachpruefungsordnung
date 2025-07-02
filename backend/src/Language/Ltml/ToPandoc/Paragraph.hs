{-# LANGUAGE TupleSections #-}

module Language.Ltml.ToPandoc.Paragraph
    ( paragraphW
    )
where

import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Paragraph (Paragraph (Paragraph))
import Language.Ltml.ToPandoc (ToPandoc)
import Language.Ltml.ToPandoc.Label (mLabelW)
import Language.Ltml.ToPandoc.Text (textBlockW)
import qualified Text.Pandoc.Definition as P (Block (Div))

-- TODO: The `div` should likely be a `section`.
-- TODO: Numbering of paragraphs.
paragraphW :: Node Paragraph -> ToPandoc P.Block
paragraphW (Node mLabel (Paragraph _ xs)) = P.Div <$> attrW <*> textBlockW xs
  where
    attrW = (,[],[]) <$> mLabelW mLabel
