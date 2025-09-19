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
import Language.Ltml.Common (Flagged')
import Language.Ltml.Tree (FlaggedInputTree', InputTree', Tree (Leaf, Tree))
import Language.Ltml.Tree.Parser
    ( TreeParser
    , disjNFlaggedTreePF
    , nFlaggedTreePF
    )
import Language.Ltml.Tree.Parser.Document (documentTXP')

appendixSectionTP
    :: NamedType AppendixSectionType
    -> FlaggedInputTree'
    -> TreeParser (Flagged' AppendixSection)
appendixSectionTP = nFlaggedTreePF aux
  where
    aux :: AppendixSectionType -> InputTree' -> TreeParser AppendixSection
    aux _ (Leaf _) = fail "Appendix section node is leaf"
    aux _ (Tree (Just _) _) = fail "Appendix section node has header"
    aux (AppendixSectionType fmt (Star t)) (Tree Nothing trees) =
        AppendixSection fmt <$> mapM (disjNFlaggedTreePF documentTXP' t) trees
