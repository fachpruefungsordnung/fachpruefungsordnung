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
import Data.List (find)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.SimpleRegex (Disjunction (Disjunction))
import Language.Lsd.AST.Type
    ( NamedType
    , ProperNodeKind (kindNameOf, typeNameOf)
    , RawProperNodeKind
    , unwrapNT
    )
import Language.Ltml.Common (Flagged, Parsed)
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nSc)
import Language.Ltml.Parser.Footnote
    ( FootnoteParser
    , FootnoteWriterT
    , eitherMapFootnoteWriterT
    )
import Language.Ltml.Tree (FlaggedTree, Tree, TypedTree (TypedTree))
import Text.Megaparsec (eof, runParser)

newtype TreeParser a = TreeParser (Either TreeError a)
    deriving (Functor, Applicative, Monad)

runTreeParser :: TreeParser a -> Either TreeError a
runTreeParser (TreeParser x) = x

class (MonadFail m) => MonadTreeParser m where
    treeParser :: Either TreeError a -> m a

instance MonadFail TreeParser where
    fail = TreeParser . Left . TreeError

instance MonadTreeParser TreeParser where
    treeParser = TreeParser

-- | An error that occurred parsing a tree's structure (not input text).
--   The frontend should prohibit such errors, and thus treat such errors as
--   internal / as bugs.
newtype TreeError = TreeError String
    deriving (Show)

type FootnoteTreeParser = FootnoteWriterT TreeParser

instance (MonadTreeParser m) => MonadTreeParser (FootnoteWriterT m) where
    treeParser = lift . treeParser

parseLeaf :: Parser a -> Text -> Parsed a
parseLeaf p x = runParser (nSc *> p <* eof) "" (x <> "\n")

leafParser :: Parser a -> Text -> TreeParser (Parsed a)
leafParser p x = return $ parseLeaf p x

{-# ANN
    leafFootnoteParser
    ("HLint: ignore Avoid lambda using `infix`" :: String)
    #-}
leafFootnoteParser :: FootnoteParser a -> Text -> FootnoteTreeParser (Parsed a)
leafFootnoteParser p x = eitherMapFootnoteWriterT (\p' -> parseLeaf p' x) p

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
            then fail $ "Invalid kind " ++ show kindName
            else case f typeName of
                Nothing -> fail $ "Invalid type " ++ show typeName
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
