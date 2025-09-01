{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Ltml.Tree.Parser
    ( TreeParser
    , FootnoteTreeParser
    , runTreeParser
    , MonadTreeParser (treeParser)
    , TreeError (..)
    , treeError
    , leafError
    , leafParser
    , leafFootnoteParser
    , flaggedTreePF
    , nFlaggedTreePF
    , disjFlaggedTreePF
    , disjNFlaggedTreePF
    )
where

import Control.Functor.Utils (traverseF)
import Control.Monad.Trans.Class (lift)
import Data.List (find, singleton)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Void (Void)
import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.SimpleRegex (Disjunction (Disjunction))
import Language.Lsd.AST.Type
    ( NamedType
    , ProperNodeKind (kindNameOf, typeNameOf)
    , RawProperNodeKind
    , unwrapNT
    )
import Language.Ltml.Common (Flagged)
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nSc)
import Language.Ltml.Parser.Footnote
    ( FootnoteParser
    , FootnoteWriterT
    , mapFootnoteWriterT
    )
import Language.Ltml.Tree (FlaggedTree, Tree, TypedTree (TypedTree))
import Text.Megaparsec (ParseErrorBundle, runParser)

newtype TreeParser a = TreeParser (Either TreeError a)
    deriving (Functor, Applicative, Monad)

runTreeParser :: TreeParser a -> Either TreeError a
runTreeParser (TreeParser x) = x

class (Monad m) => MonadTreeParser m where
    treeParser :: Either TreeError a -> m a

instance MonadTreeParser TreeParser where
    treeParser = TreeParser

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

type FootnoteTreeParser = FootnoteWriterT TreeParser

instance (MonadTreeParser m) => MonadTreeParser (FootnoteWriterT m) where
    treeParser = lift . treeParser

treeError :: (MonadTreeParser m) => String -> m a
treeError = treeParser . Left . TreeError . singleton

leafError :: (MonadTreeParser m) => ParseErrorBundle Text Void -> m a
leafError = treeParser . Left . LeafError

leafParser :: Parser a -> Text -> TreeParser a
leafParser p x =
    case runParser (nSc *> p) "" (x <> "\n") of
        Left e -> leafError e
        Right y -> return y

{-# ANN
    leafFootnoteParser
    ("HLint: ignore Avoid lambda using `infix`" :: String)
    #-}
leafFootnoteParser :: FootnoteParser a -> Text -> FootnoteTreeParser a
leafFootnoteParser p x = mapFootnoteWriterT (\p' -> leafParser p' x) p

flaggedTreePF'
    :: (MonadTreeParser m, ProperNodeKind t)
    => (TypeName -> Maybe (Tree flag a b -> m c))
    -> Proxy t
    -> FlaggedTree flag a b
    -> m (Flagged flag c)
flaggedTreePF' f kind = traverseF aux
  where
    aux (TypedTree kindName typeName tree) =
        if kindName /= kindNameOf kind
            then treeError "Invalid kind"
            else case f typeName of
                Nothing -> treeError "Invalid type"
                Just f' -> f' tree

flaggedTreePF
    :: forall m t flag a b c
     . (MonadTreeParser m, ProperNodeKind t)
    => (t -> Tree flag a b -> m c)
    -> t
    -> FlaggedTree flag a b
    -> m (Flagged flag c)
flaggedTreePF f t = flaggedTreePF' f' (Proxy :: Proxy t)
  where
    f' typeName =
        if typeName == typeNameOf t
            then Just $ f t
            else Nothing

nFlaggedTreePF
    :: (MonadTreeParser m, RawProperNodeKind t)
    => (t -> Tree flag a b -> m c)
    -> NamedType t
    -> FlaggedTree flag a b
    -> m (Flagged flag c)
nFlaggedTreePF f = flaggedTreePF (f . unwrapNT)

disjFlaggedTreePF
    :: forall m t flag a b c
     . (MonadTreeParser m, ProperNodeKind t)
    => (t -> Tree flag a b -> m c)
    -> Disjunction t
    -> FlaggedTree flag a b
    -> m (Flagged flag c)
disjFlaggedTreePF f (Disjunction ts) = flaggedTreePF' f' (Proxy :: Proxy t)
  where
    f' typeName = f <$> find ((typeName ==) . typeNameOf) ts

disjNFlaggedTreePF
    :: (MonadTreeParser m, RawProperNodeKind t)
    => (t -> Tree flag a b -> m c)
    -> Disjunction (NamedType t)
    -> FlaggedTree flag a b
    -> m (Flagged flag c)
disjNFlaggedTreePF f = disjFlaggedTreePF (f . unwrapNT)
