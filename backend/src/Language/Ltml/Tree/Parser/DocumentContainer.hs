module Language.Ltml.Tree.Parser.DocumentContainer
    ( documentContainerTP
    )
where

import Data.Text (Text)
import Language.Lsd.AST.SimpleRegex (Disjunction, Sequence (Sequence))
import Language.Lsd.AST.Type (NamedType)
import Language.Lsd.AST.Type.AppendixSection (AppendixSectionType)
import Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerType (DocumentContainerType)
    )
import Language.Ltml.AST.AppendixSection (AppendixSection)
import Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (DocumentContainer)
    , DocumentContainerHeader
    )
import Language.Ltml.Common (Flagged')
import Language.Ltml.Parser.DocumentContainer (documentContainerHeaderP)
import Language.Ltml.Tree (FlaggedInputTree', InputTree', Tree (Leaf, Tree))
import Language.Ltml.Tree.Parser
    ( TreeParser
    , disjNFlaggedTreePF
    , leafParser
    , treeError
    )
import Language.Ltml.Tree.Parser.AppendixSection (appendixSectionTP)
import Language.Ltml.Tree.Parser.Document (documentTP)

documentContainerTP
    :: Disjunction (NamedType DocumentContainerType)
    -> FlaggedInputTree'
    -> TreeParser (Flagged' DocumentContainer)
documentContainerTP = disjNFlaggedTreePF aux
  where
    aux :: DocumentContainerType -> InputTree' -> TreeParser DocumentContainer
    aux _ (Leaf _) = treeError "Document container node is leaf"
    aux _ (Tree _ []) =
        treeError "Document container lacks main document child"
    aux
        (DocumentContainerType fmt mainDocT appsT)
        (Tree x (mainDocTree : trees)) =
            DocumentContainer fmt
                <$> headerTP x
                <*> documentTP mainDocT mainDocTree
                <*> appendicesTP appsT trees

headerTP :: Maybe Text -> TreeParser DocumentContainerHeader
headerTP Nothing = treeError "Document container lacks header"
headerTP (Just x) = leafParser documentContainerHeaderP x

appendicesTP
    :: Sequence (NamedType AppendixSectionType)
    -> [FlaggedInputTree']
    -> TreeParser [Flagged' AppendixSection]
appendicesTP (Sequence ts) tTrees =
    case safeZipWith appendixSectionTP ts tTrees of
        Nothing -> treeError "Wrong number of appendix sections"
        Just apps -> sequence apps

safeZipWith :: (a -> b -> c) -> [a] -> [b] -> Maybe [c]
safeZipWith _ [] [] = return []
safeZipWith f (x : xs) (y : ys) = (f x y :) <$> safeZipWith f xs ys
safeZipWith _ _ _ = Nothing
