{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ltml.Parser
    ( Parser
    , MonadParser
    , ParserWrapper (wrapParser)
    )
where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
    ( MonadParsec
    , Parsec
    )

type Parser = Parsec Void Text

type MonadParser m = (MonadParsec Void Text m, MonadFail m)

class (MonadParser m) => ParserWrapper m where
    wrapParser :: Parser a -> m a

instance ParserWrapper Parser where
    wrapParser = id
