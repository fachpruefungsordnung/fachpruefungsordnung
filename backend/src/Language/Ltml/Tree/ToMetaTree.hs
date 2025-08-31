{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Ltml.Tree.ToMetaTree
    ( buildMetaTree
    )
where

import Control.Functor.Utils (traverseF)
import Control.Monad.ConsumableStack
    ( ConsumableStackT
    , pop
    , runConsumableStackT
    )
import Data.Map (Map, lookup)
import Language.Lsd.AST.Common (FullTypeName)
import Language.Lsd.AST.Type (kindHasTocHeading, properTypeCollect')
import Language.Lsd.Example.Fpo (fpoT)
import Language.Ltml.Tree
    ( FlaggedInputTree
    , FlaggedMetaTree
    , HtmlHeading
    , InputTree
    , MetaTree
    , Tree (Leaf, Tree)
    , TypedInputTree
    , TypedMetaTree
    , TypedTree (TypedTree)
    )
import Prelude hiding (lookup)

-- | Build a meta tree.
--   Headings must be given in pre-order.
--   Returns @Left msg@ on an error, which is never a user error, but rather a
--   bug.
--   This does not fully check the validity of the input tree, which is done
--   elsewhere (specifically, by 'Language.Ltml.Tree.ToLtml.treeToLtml').
buildMetaTree
    :: [HtmlHeading]
    -> FlaggedInputTree ident
    -> Either String (FlaggedMetaTree ident)
buildMetaTree hs tree =
    runEitherFail $ runConsumableStackT (flaggedTreeF tree) hs

-- TODO: Do not hard-code (`fpoT`).
hasHeadingMap :: Map FullTypeName Bool
hasHeadingMap = properTypeCollect' (kindHasTocHeading . pure) fpoT

newtype EitherFail a = EitherFail {runEitherFail :: Either String a}
    deriving (Functor, Applicative, Monad)

instance MonadFail EitherFail where
    fail = EitherFail . Left

type HeadingStack = ConsumableStackT HtmlHeading EitherFail

flaggedTreeF :: FlaggedInputTree ident -> HeadingStack (FlaggedMetaTree ident)
flaggedTreeF = traverseF typedTreeF

typedTreeF :: TypedInputTree ident -> HeadingStack (TypedMetaTree ident)
typedTreeF (TypedTree kindName typeName tree) =
    case lookup (kindName, typeName) hasHeadingMap of
        Just b -> TypedTree kindName typeName <$> treeF b tree
        Nothing -> fail $ "Unknown type: " ++ show (kindName, typeName)

treeF :: Bool -> InputTree ident -> HeadingStack (MetaTree ident)
treeF hasHeading = aux
  where
    aux (Tree _ trees) =
        Tree <$> headingF hasHeading <*> mapM flaggedTreeF trees
    aux (Leaf _) = Leaf <$> headingF hasHeading

    headingF True = Just <$> pop
    headingF False = pure Nothing
