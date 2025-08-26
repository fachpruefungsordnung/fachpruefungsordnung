module Language.Ltml.Tree.Parser.AppendixSection
    ( appendixSectionTP
    )
where

import Language.Lsd.AST.SimpleRegex (Star (Star))
import Language.Lsd.AST.Type (NamedType)
import Language.Lsd.AST.Type.AppendixSection
    ( AppendixSectionType (AppendixSectionType)
    )
import Language.Ltml.AST.AppendixSection (AppendixSection (AppendixSection))
import Language.Ltml.Tree (Tree (Leaf, Tree), TypedTree)
import Language.Ltml.Tree.Parser
    ( TreeParser
    , disjNTypedTreePF
    , nTypedTreePF
    , treeError
    )
import Language.Ltml.Tree.Parser.Document (documentTXP')

appendixSectionTP
    :: NamedType AppendixSectionType
    -> TypedTree
    -> TreeParser AppendixSection
appendixSectionTP = nTypedTreePF aux
  where
    aux :: AppendixSectionType -> Tree -> TreeParser AppendixSection
    aux _ (Leaf _) = treeError "Appendix section node is leaf"
    aux _ (Tree (Just _) _) = treeError "Appendix section node has header"
    aux (AppendixSectionType fmt (Star t)) (Tree Nothing trees) =
        AppendixSection fmt <$> mapM (disjNTypedTreePF documentTXP' t) trees
