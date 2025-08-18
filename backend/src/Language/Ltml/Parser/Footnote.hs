{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Ltml.Parser.Footnote
    ( FootnoteParser
    , unwrapFootnoteParser
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
import Language.Ltml.Parser.Text (lHangingTextP)

type FootnoteParser =
    ReaderT [FootnoteType] (StateT (Map Label Footnote) Parser)

instance ParserWrapper FootnoteParser where
    wrapParser = lift . lift

unwrapFootnoteParser
    :: [FootnoteType]
    -> FootnoteParser a
    -> Parser (a, Map Label Footnote)
unwrapFootnoteParser ts p = runStateT (runReaderT p ts) Map.empty

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
    fmap (Footnote fmt) <$> lHangingTextP kw tt
