{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Ltml.Parser.Footnote
    ( FootnoteParser
    , FootnotePT
    , unwrapFootnoteParser
    , unwrapFootnoteParser'
    , footnoteP
    )
where

import Control.Applicative.Combinators (choice)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import Data.Map.Utils (insert')
import Data.Text (unpack)
import Language.Lsd.AST.Type.Footnote (FootnoteType (FootnoteType))
import Language.Ltml.AST.Footnote (Footnote (Footnote))
import Language.Ltml.AST.Label (Label (unLabel))
import Language.Ltml.Parser (Parser, ParserWrapper (wrapParser))
import Language.Ltml.Parser.Text (hangingTextP')

type FootnotePT m =
    ReaderT [FootnoteType] (StateT (Map Label Footnote) m)

type FootnoteParser = FootnotePT Parser

instance ParserWrapper FootnoteParser where
    wrapParser = lift . lift

unwrapFootnoteParser
    :: [FootnoteType]
    -> FootnotePT m a
    -> m (a, Map Label Footnote)
unwrapFootnoteParser ts = unwrapFootnoteParser' ts Map.empty

unwrapFootnoteParser'
    :: [FootnoteType]
    -> Map Label Footnote
    -> FootnotePT m a
    -> m (a, Map Label Footnote)
unwrapFootnoteParser' ts fnMap p = runStateT (runReaderT p ts) fnMap

footnoteP :: FootnoteParser ()
footnoteP =
    ask >>= wrapParser . choice . fmap footnoteP' >>= uncurry add
  where
    add :: Label -> Footnote -> FootnoteParser ()
    add label fn = do
        fnMap <- get
        case insert' label fn fnMap of
            Nothing ->
                fail $
                    "Footnote {"
                        ++ unpack (unLabel label)
                        ++ "} already defined."
            Just fnMap' -> put fnMap'

footnoteP' :: FootnoteType -> Parser (Label, Footnote)
footnoteP' (FootnoteType kw fmt tt) =
    fmap (Footnote fmt) <$> hangingTextP' kw tt
