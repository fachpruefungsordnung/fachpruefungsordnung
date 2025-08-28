{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Ltml.Tree.Parser
    ( TreeParser
    , runTreeParser
    , TreeParserWrapper (wrapTreeParser)
    , TreeError (..)
    , treeError
    , leafError
    , leafParser
    , nFlaggedTreePF
    , staticFlaggedTreePF
    , disjNFlaggedTreePF
    , disjStaticFlaggedTreePF
    )
where

import Control.Functor.Utils (traverseF)
import Data.List (find)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Void (Void)
import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.SimpleRegex (Disjunction (Disjunction))
import Language.Lsd.AST.Type
    ( KindNameOf (kindNameOf)
    , NamedType (ntTypeName, unwrapNT)
    , TypeNameOf (typeNameOf)
    )
import Language.Ltml.Common (Flagged)
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nSc)
import Language.Ltml.Tree (FlaggedTree, Tree, TypedTree (TypedTree))
import Text.Megaparsec (ParseErrorBundle, runParser)

newtype TreeParser a = TreeParser (Either TreeError a)
    deriving (Functor, Applicative, Monad)

runTreeParser :: TreeParser a -> Either TreeError a
runTreeParser (TreeParser x) = x

-- CONSIDER: Merge with ParserWrapper.
class (Monad m) => TreeParserWrapper m where
    wrapTreeParser :: TreeParser a -> m a

instance TreeParserWrapper TreeParser where
    wrapTreeParser = id

-- | An error that occurred parsing a tree.
data TreeError
    = -- | An error that occurred parsing a leaf.
      --   Such an error should always be a user error.
      LeafError (ParseErrorBundle Text Void)
    | -- | An error that occurred parsing the tree's structure.
      --   The frontend should prohibit such errors, and thus treat such errors
      --   as internal / as bugs.
      TreeError [String]
    deriving (Show)

treeError :: (TreeParserWrapper m) => String -> m a
treeError = wrapTreeParser . TreeParser . Left . TreeError . pure

leafError :: (TreeParserWrapper m) => ParseErrorBundle Text Void -> m a
leafError = wrapTreeParser . TreeParser . Left . LeafError

leafParser :: Parser a -> Text -> TreeParser a
leafParser p x =
    case runParser (nSc *> p) "" (x <> "\n") of
        Left e -> leafError e
        Right y -> return y

flaggedTreePF
    :: (TreeParserWrapper m, KindNameOf t)
    => (TypeName -> Tree -> m a)
    -> Proxy t
    -> FlaggedTree
    -> m (Flagged a)
flaggedTreePF f kind = traverseF aux
  where
    aux (TypedTree kindName typeName tree) =
        if kindName == kindNameOf kind
            then f typeName tree
            else treeError "Invalid kind"

flaggedTreePF'
    :: forall m t a
     . (TreeParserWrapper m, KindNameOf t)
    => (Tree -> m a)
    -> t
    -> TypeName
    -> FlaggedTree
    -> m (Flagged a)
flaggedTreePF' f _ typeName = flaggedTreePF f' (Proxy :: Proxy t)
  where
    f' typeName' tree =
        if typeName' == typeName
            then f tree
            else treeError "Invalid type"

nFlaggedTreePF
    :: (TreeParserWrapper m, KindNameOf t)
    => (t -> Tree -> m a)
    -> NamedType t
    -> FlaggedTree
    -> m (Flagged a)
nFlaggedTreePF f nt = flaggedTreePF' (f t) t (ntTypeName nt)
  where
    t = unwrapNT nt

staticFlaggedTreePF
    :: (TreeParserWrapper m, KindNameOf t, TypeNameOf t)
    => (t -> Tree -> m a)
    -> t
    -> FlaggedTree
    -> m (Flagged a)
staticFlaggedTreePF f t = flaggedTreePF' (f t) t (typeNameOf t)

disjFlaggedTreePF
    :: forall m t a
     . (TreeParserWrapper m, KindNameOf t)
    => (t -> TypeName)
    -> (t -> Tree -> m a)
    -> Disjunction t
    -> FlaggedTree
    -> m (Flagged a)
disjFlaggedTreePF getTypeName f (Disjunction ts) =
    flaggedTreePF f' (Proxy :: Proxy t)
  where
    f' typeName tree =
        case find ((typeName ==) . getTypeName) ts of
            Just t -> f t tree
            Nothing -> treeError "Invalid type"

disjNFlaggedTreePF
    :: (TreeParserWrapper m, KindNameOf t)
    => (t -> Tree -> m a)
    -> Disjunction (NamedType t)
    -> FlaggedTree
    -> m (Flagged a)
disjNFlaggedTreePF f = disjFlaggedTreePF ntTypeName (f . unwrapNT)

disjStaticFlaggedTreePF
    :: (TreeParserWrapper m, KindNameOf t, TypeNameOf t)
    => (t -> Tree -> m a)
    -> Disjunction t
    -> FlaggedTree
    -> m (Flagged a)
disjStaticFlaggedTreePF = disjFlaggedTreePF typeNameOf
