module Language.Ltml.Tree.ToLtml
    ( treeToLtml
    )
where

import Language.Lsd.AST.SimpleRegex (Disjunction (Disjunction))
import Language.Lsd.Example.Fpo (fpoT)
import Language.Ltml.AST.DocumentContainer (DocumentContainer)
import Language.Ltml.Tree (TypedTree)
import Language.Ltml.Tree.Parser (TreeError, runTreeParser)
import Language.Ltml.Tree.Parser.DocumentContainer (documentContainerTP)

treeToLtml :: TypedTree -> Either TreeError DocumentContainer
treeToLtml tTree = runTreeParser $ documentContainerTP t tTree
  where
    t = Disjunction [fpoT] -- TODO: Do not hard-code.
