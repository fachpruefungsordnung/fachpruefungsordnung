{-# LANGUAGE TupleSections #-}

module Language.Ltml.ToPandoc.Section
    ( sectionW
    )
where

import Control.Applicative.Utils ((<:>))
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Section (Heading (Heading), Section (Section))
import Language.Ltml.ToPandoc (ToPandoc)
import Language.Ltml.ToPandoc.Label (mLabelW)
import Language.Ltml.ToPandoc.Paragraph (paragraphW)
import Language.Ltml.ToPandoc.Text (inlineTextW)
import qualified Text.Pandoc.Definition as P (Block (Div, Header))

-- TODO: The `div` should likely be a `section`.
-- TODO: Do not ignore format.
sectionW :: Node Section -> ToPandoc P.Block
sectionW (Node mLabel (Section _ heading children)) =
    P.Div <$> attrW <*> (headingW heading <:> childrenW children)
  where
    attrW = (,[],[]) <$> mLabelW mLabel

    childrenW (Left pars) = mapM paragraphW pars
    childrenW (Right secs) = mapM sectionW secs

-- TODO: Do not ignore format.
-- TODO: Proper heading level.
headingW :: Heading -> ToPandoc P.Block
headingW (Heading _ xs) = P.Header 1 (mempty, [], []) <$> inlineTextW xs
