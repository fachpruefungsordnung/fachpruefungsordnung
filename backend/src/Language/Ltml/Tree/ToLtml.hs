module Language.Ltml.Tree.ToLtml
    ( treeToLtml
    )
where

import Language.Lsd.AST.SimpleRegex (Disjunction (Disjunction))
import Language.Lsd.Example.Fpo (fpoT)
import Language.Ltml.AST.DocumentContainer (DocumentContainer)
import Language.Ltml.Common (Flagged')
import Language.Ltml.Tree (FlaggedInputTree')
import Language.Ltml.Tree.Parser (TreeError, runTreeParser)
import Language.Ltml.Tree.Parser.DocumentContainer (documentContainerTP)

-- | Convert an input tree to an LTML AST.
--   The boolean flags denote which part(s) of the tree output should be
--   generated for.
treeToLtml
    :: FlaggedInputTree'
    -> Either TreeError (Flagged' DocumentContainer)
treeToLtml tTree = runTreeParser $ documentContainerTP t tTree
  where
    t = Disjunction [fpoT] -- TODO: Do not hard-code.
