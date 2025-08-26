{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Ltml.Tree.Parser
    ( TreeParser
    , runTreeParser
    , TreeParserWrapper (wrapTreeParser)
    , treeError
    , leafError
    , leafParser
    , typedTreePF
    , typedTreePF'
    , nTypedTreePF
    , staticTypedTreePF
    , disjTypedTreePF
    , disjNTypedTreePF
    , disjStaticTypedTreePF
    )
where

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
import Language.Ltml.Common (Flagged (Flagged))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nSc)
import Language.Ltml.Tree (Tree, TypedTree (TypedTree))
import Text.Megaparsec (ParseErrorBundle, runParser)

newtype TreeParser a = TreeParser (Either TreeError a)
    deriving (Functor, Applicative, Monad)

runTreeParser :: TreeParser a -> Either TreeError a
runTreeParser (TreeParser x) = x

-- CONSIDER: Merge with ParserWrapper.
class TreeParserWrapper m where
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

leafParser :: Parser a -> Flagged Text -> TreeParser (Flagged a)
leafParser p (Flagged f x) =
    case runParser (nSc *> p) "" (x <> "\n") of
        Left e -> leafError e
        Right y -> return (Flagged f y)

typedTreePF
    :: (TreeParserWrapper m, KindNameOf t)
    => (TypeName -> Tree -> m a)
    -> Proxy t
    -> TypedTree
    -> m a
typedTreePF f kind (TypedTree kindName typeName tree) =
    if kindName == kindNameOf kind
        then f typeName tree
        else treeError "Invalid kind"

typedTreePF'
    :: forall m t a
     . (TreeParserWrapper m, KindNameOf t)
    => (Tree -> m a)
    -> t
    -> TypeName
    -> TypedTree
    -> m a
typedTreePF' f _ typeName = typedTreePF f' (Proxy :: Proxy t)
  where
    f' typeName' tree =
        if typeName' == typeName
            then f tree
            else treeError "Invalid type"

nTypedTreePF
    :: (TreeParserWrapper m, KindNameOf t)
    => (t -> Tree -> m a)
    -> NamedType t
    -> TypedTree
    -> m a
nTypedTreePF f nt = typedTreePF' (f t) t (ntTypeName nt)
  where
    t = unwrapNT nt

staticTypedTreePF
    :: (TreeParserWrapper m, KindNameOf t, TypeNameOf t)
    => (t -> Tree -> m a)
    -> t
    -> TypedTree
    -> m a
staticTypedTreePF f t = typedTreePF' (f t) t (typeNameOf t)

disjTypedTreePF
    :: forall m t a
     . (TreeParserWrapper m, KindNameOf t)
    => (t -> TypeName)
    -> (t -> Tree -> m a)
    -> Disjunction t
    -> TypedTree
    -> m a
disjTypedTreePF getTypeName f (Disjunction ts) =
    typedTreePF f' (Proxy :: Proxy t)
  where
    f' typeName tree =
        case find ((typeName ==) . getTypeName) ts of
            Just t -> f t tree
            Nothing -> treeError "Invalid type"

disjNTypedTreePF
    :: (TreeParserWrapper m, KindNameOf t)
    => (t -> Tree -> m a)
    -> Disjunction (NamedType t)
    -> TypedTree
    -> m a
disjNTypedTreePF f = disjTypedTreePF ntTypeName (f . unwrapNT)

disjStaticTypedTreePF
    :: (TreeParserWrapper m, KindNameOf t, TypeNameOf t)
    => (t -> Tree -> m a)
    -> Disjunction t
    -> TypedTree
    -> m a
disjStaticTypedTreePF = disjTypedTreePF typeNameOf
