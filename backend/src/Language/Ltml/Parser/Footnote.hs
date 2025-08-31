{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Ltml.Parser.Footnote
    ( FootnoteParser
    , FootnoteWriterT
    , runFootnoteWriterT
    , mapFootnoteWriterT
    , footnoteP
    )
where

import Control.Applicative (Alternative)
import Control.Applicative.Combinators (choice)
import Control.Monad (MonadPlus)
import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Control.Monad.State (StateT, get, mapStateT, put, runStateT)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import Data.Map.Utils (insert')
import Data.Text (Text, unpack)
import Data.Void (Void)
import Language.Lsd.AST.Type.Footnote (FootnoteType (FootnoteType))
import Language.Ltml.AST.Footnote (Footnote (Footnote))
import Language.Ltml.AST.Label (Label (unLabel))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Text (hangingTextP')
import Text.Megaparsec (MonadParsec)

type FootnoteMap = Map Label Footnote

newtype FootnoteWriterT m a
    = FootnoteWriterT (ReaderT [FootnoteType] (StateT FootnoteMap m) a)
    deriving (Functor, Applicative, Monad, MonadFail, Alternative, MonadPlus)

instance MonadTrans FootnoteWriterT where
    lift = FootnoteWriterT . lift . lift

type FootnoteParser = FootnoteWriterT Parser

deriving instance
    (MonadParsec Void Text m)
    => MonadParsec Void Text (FootnoteWriterT m)

mapFootnoteWriterT
    :: (m (a, FootnoteMap) -> n (b, FootnoteMap))
    -> FootnoteWriterT m a
    -> FootnoteWriterT n b
mapFootnoteWriterT f (FootnoteWriterT p) =
    FootnoteWriterT $ mapReaderT (mapStateT f) p

runFootnoteWriterT
    :: FootnoteWriterT m a
    -> [FootnoteType]
    -> m (a, FootnoteMap)
runFootnoteWriterT (FootnoteWriterT p) ts =
    runStateT (runReaderT p ts) Map.empty

footnoteP :: FootnoteParser ()
footnoteP =
    FootnoteWriterT ask >>= lift . choice . fmap footnoteP' >>= uncurry add
  where
    add :: Label -> Footnote -> FootnoteParser ()
    add label fn = do
        fnMap <- FootnoteWriterT get
        case insert' label fn fnMap of
            Nothing ->
                fail $
                    "Footnote {"
                        ++ unpack (unLabel label)
                        ++ "} already defined."
            Just fnMap' -> FootnoteWriterT $ put fnMap'

footnoteP' :: FootnoteType -> Parser (Label, Footnote)
footnoteP' (FootnoteType kw fmt tt) =
    fmap (Footnote fmt) <$> hangingTextP' kw tt
