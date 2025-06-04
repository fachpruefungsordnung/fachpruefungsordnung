{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Common.SimpleRegex
    ( simpleRegexP
    )
where

import Control.Applicative.Combinators (choice)
import Language.Lsd.AST.SimpleRegex
    ( Disjunction (..)
    , Sequence (..)
    , SimpleRegex (..)
    , Star (..)
    )
import Language.Ltml.Parser (MonadParser)
import Text.Megaparsec (many)

simpleRegexP :: (MonadParser m) => (t -> m a) -> SimpleRegex t -> m [a]
simpleRegexP f (SimpleRegex prefix middle suffix) =
    concat
        <$> sequence
            [ sequenceP f prefix
            , middleP f middle
            , sequenceP f suffix
            ]

starP :: (MonadParser m) => (t -> m a) -> Star t -> m [a]
starP f (Star t) = many $ f t

disjunctionP :: (MonadParser m) => (t -> m a) -> Disjunction t -> m a
disjunctionP f (Disjunction ts) = choice (fmap f ts)

sequenceP :: (MonadParser m) => (t -> m a) -> Sequence t -> m [a]
sequenceP f (Sequence ts) = mapM f ts

middleP
    :: (MonadParser m)
    => (t -> m a)
    -> Disjunction (Star (Disjunction t))
    -> m [a]
middleP = disjunctionP . starP . disjunctionP
