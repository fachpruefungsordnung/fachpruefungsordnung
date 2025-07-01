{-# LANGUAGE TupleSections #-}

module Language.Ltml.ToPandoc.Section
    ( sectionW
    )
where

import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Section (Heading (Heading), Section (Section))
import Language.Ltml.ToPandoc (ToPandoc, headed)
import Language.Ltml.ToPandoc.Label (mLabelW)
import Language.Ltml.ToPandoc.Paragraph (paragraphW)
import Language.Ltml.ToPandoc.Text (inlineTextW)
import qualified Text.Pandoc.Definition as P
    ( Attr
    , Block (Div)
    , Inline
    )

-- TODO: The `div` should likely be a `section`.
-- TODO: Do not ignore format.
sectionW :: Node Section -> ToPandoc P.Block
sectionW (Node mLabel (Section _ heading children)) =
    P.Div <$> attrW <*> (headed (headingW heading) (childrenW children))
  where
    attrW = (,[],[]) <$> mLabelW mLabel

    childrenW (Left pars) = mapM paragraphW pars
    childrenW (Right secs) = mapM sectionW secs

-- TODO: Do not ignore format.
headingW :: Heading -> ToPandoc (P.Attr, [P.Inline])
headingW (Heading _ xs) = ((mempty, [], []),) <$> inlineTextW xs
