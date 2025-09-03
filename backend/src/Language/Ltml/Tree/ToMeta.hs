{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Ltml.Tree.ToMeta
    ( MetaError (..)
    , treeToMeta
    )
where

import Control.Functor.Utils (traverseF)
import Control.Monad.ConsumableStack
    ( ConsumableStackT
    , pop
    , runConsumableStackT
    )
import Data.Bifunctor (first)
import Data.List (find)
import Data.Map (Map, lookup)
import Data.Text (Text)
import Language.Lsd.AST.Common (FullTypeName)
import Language.Lsd.AST.Type
    ( NamedType
    , NavHeadingGeneration (NavHeadingFromHtmlToc, NavHeadingStatic)
    , ProperTypeMeta
    , fullTypeNameOf
    , navHeadingGenerationOf
    , properTypeCollect'
    )
import Language.Lsd.AST.Type.DocumentContainer (DocumentContainerType)
import Language.Lsd.Example (availableLSDs)
import Language.Lsd.ToMetaMap (buildMetaMap)
import Language.Ltml.Common (Flagged (Flagged))
import Language.Ltml.HTML (mkRenderedTocEntry, renderTocList)
import Language.Ltml.HTML.Common (RenderedTocEntry)
import Language.Ltml.Tree
    ( FlaggedInputTree
    , FlaggedMetaTree
    , InputTree
    , MetaTree
    , Tree (Leaf, Tree)
    , TypedInputTree
    , TypedMetaTree
    , TypedTree (TypedTree)
    , flaggedTreeMap
    )
import Language.Ltml.Tree.Parser (TreeError)
import Language.Ltml.Tree.ToLtml (treeToLtml)
import Prelude hiding (lookup)

data MetaError
    = MetaBug String
    | MetaTreeError TreeError
    deriving (Show)

-- | Build metadata, to be sent to the frontend.
treeToMeta
    :: FlaggedInputTree ident
    -> Either
        MetaError
        ( FlaggedMetaTree ident
        , Map FullTypeName ProperTypeMeta
        )
treeToMeta tree = do
    ast <- first MetaTreeError $ treeToLtml tree'
    let headings = renderTocList ast
    first MetaBug $ buildMeta' headings tree
  where
    tree' :: FlaggedInputTree Bool
    tree' = flaggedTreeMap (const dummyFlag) id id tree
      where
        dummyFlag = True -- ignored; should never be evaluated

-- | Build metadata, based on a list of headings and a tree.
--   Headings must be given in pre-order.
--   Returns @Left msg@ on an error, which is never a user error, but rather a
--   bug.
--   This does not fully check the validity of the input tree, which is done
--   elsewhere (specifically, by 'Language.Ltml.Tree.ToLtml.treeToLtml').
buildMeta'
    :: [RenderedTocEntry]
    -> FlaggedInputTree ident
    -> Either String (FlaggedMetaTree ident, Map FullTypeName ProperTypeMeta)
buildMeta' hs tree = do
    t <- getRootType tree
    metaTree <- buildMetaTree t hs tree
    let metaMap = buildMetaMap t
    return (metaTree, metaMap)
  where
    getRootType (Flagged _ (TypedTree kindName typeName _)) =
        case find ((== fullTypeName) . fullTypeNameOf) availableLSDs of
            Just t -> Right t
            Nothing -> Left $ "Unknown type: " ++ show fullTypeName
      where
        fullTypeName = (kindName, typeName)

newtype EitherFail a = EitherFail {runEitherFail :: Either String a}
    deriving (Functor, Applicative, Monad)

instance MonadFail EitherFail where
    fail = EitherFail . Left

type HeadingStack = ConsumableStackT RenderedTocEntry EitherFail

buildMetaTree
    :: NamedType DocumentContainerType
    -> [RenderedTocEntry]
    -> FlaggedInputTree ident
    -> Either String (FlaggedMetaTree ident)
buildMetaTree t hs tree0 =
    runEitherFail $ runConsumableStackT (flaggedTreeF tree0) hs
  where
    headingGenMap :: Map FullTypeName NavHeadingGeneration
    headingGenMap = properTypeCollect' (navHeadingGenerationOf . pure) t

    flaggedTreeF
        :: FlaggedInputTree ident
        -> HeadingStack (FlaggedMetaTree ident)
    flaggedTreeF = traverseF typedTreeF

    typedTreeF :: TypedInputTree ident -> HeadingStack (TypedMetaTree ident)
    typedTreeF (TypedTree kindName typeName tree) =
        case lookup (kindName, typeName) headingGenMap of
            Just b -> TypedTree kindName typeName <$> treeF b tree
            Nothing -> fail $ "Unknown type: " ++ show (kindName, typeName)

    treeF
        :: NavHeadingGeneration
        -> InputTree ident
        -> HeadingStack (MetaTree ident)
    treeF headingGen = aux
      where
        aux (Tree _ trees) =
            Tree <$> headingF headingGen <*> mapM flaggedTreeF trees
        aux (Leaf _) = Leaf <$> headingF headingGen

    headingF :: NavHeadingGeneration -> HeadingStack RenderedTocEntry
    headingF (NavHeadingStatic x) = pure $ mkRenderedTocEntry x
    headingF NavHeadingFromHtmlToc = pop
