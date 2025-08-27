{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Ltml.Tree.Parser.Footnote
    ( FootnoteTreeParser
    , leafFootnoteParser
    )
where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Language.Ltml.Common (Flagged (Flagged))
import Language.Ltml.Parser.Common.Lexeme (nSc)
import Language.Ltml.Parser.Footnote
    ( FootnotePT
    , FootnoteParser
    , unwrapFootnoteParser'
    )
import Language.Ltml.Tree.Parser
    ( TreeParser
    , TreeParserWrapper (wrapTreeParser)
    , leafError
    )
import Text.Megaparsec (runParser)

type FootnoteTreeParser = FootnotePT TreeParser

instance TreeParserWrapper FootnoteTreeParser where
    wrapTreeParser = lift . lift

leafFootnoteParser
    :: FootnoteParser a
    -> Flagged Text
    -> FootnoteTreeParser (Flagged a)
leafFootnoteParser p (Flagged f x) = do
    env <- ask
    st <- get
    let p' = unwrapFootnoteParser' env st p
    case runParser (nSc *> p') "" (x <> "\n") of
        Left e -> leafError e
        Right (y, st') -> put st' >> return (Flagged f y)
