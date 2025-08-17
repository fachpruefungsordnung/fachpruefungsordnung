{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.Parser.Common.Combinators
    ( manyTillSucc
    )
where

import Language.Ltml.Parser (MonadParser)
import Text.Megaparsec (lookAhead, manyTill, try)

-- | Like 'manyTill', but never consume input for the @end@ parser.
manyTillSucc :: (MonadParser m) => m a -> m end -> m [a]
manyTillSucc p end = manyTill p (lookAhead $ try end)
