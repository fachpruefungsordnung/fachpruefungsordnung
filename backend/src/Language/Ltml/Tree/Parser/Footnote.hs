{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Ltml.Tree.Parser.Footnote
    ( FootnoteTreeParser
    , leafFootnoteParser
    )
where

import Control.Monad.Trans.Class (lift)
import Data.Text (Text)
import Language.Ltml.Parser.Footnote
    ( FootnoteParser
    , FootnoteWriterT
    , mapFootnoteWriterT
    )
import Language.Ltml.Tree.Parser
    ( MonadTreeParser (treeParser)
    , TreeParser
    , leafParser
    )

type FootnoteTreeParser = FootnoteWriterT TreeParser

instance (MonadTreeParser m) => MonadTreeParser (FootnoteWriterT m) where
    treeParser = lift . treeParser

{-# ANN leafFootnoteParser "HLint: ignore Avoid lambda using `infix`" #-}
leafFootnoteParser :: FootnoteParser a -> Text -> FootnoteTreeParser a
leafFootnoteParser p x = mapFootnoteWriterT (\p' -> leafParser p' x) p
