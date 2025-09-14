{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Ltml.Parser.Footnote
    ( FootnoteParser
    , FootnoteWriterT
    , runFootnoteWriterT
    , mapFootnoteWriterT
    , eitherMapFootnoteWriterT
    , footnoteP
    )
where

import Control.Applicative (Alternative)
import Control.Applicative.Combinators (choice)
import Control.Monad (MonadPlus)
import Control.Monad.Reader (ReaderT, ask, mapReaderT, runReaderT)
import Control.Monad.State (StateT, get, mapStateT, put, runStateT, state)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import Data.Map.Utils (insert')
import qualified Data.Set as Set (singleton)
import Data.Text (Text, unpack)
import Data.Void (Void)
import Language.Lsd.AST.Type.Footnote (FootnoteType (FootnoteType))
import Language.Ltml.AST.Footnote (Footnote (Footnote))
import Language.Ltml.AST.Label (Label (unLabel))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Text (hangingTextP')
import Text.Megaparsec (MonadParsec, getOffset, parseError)
import Text.Megaparsec.Error
    ( ErrorFancy (ErrorFail)
    , ParseError (FancyError)
    )

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

eitherMapFootnoteWriterT
    :: (Monad n)
    => (m (a, FootnoteMap) -> Either e (b, FootnoteMap))
    -> FootnoteWriterT m a
    -> FootnoteWriterT n (Either e b)
eitherMapFootnoteWriterT f (FootnoteWriterT m) =
    FootnoteWriterT $ mapReaderT (eitherMapStateT f) m

eitherMapStateT
    :: (Monad n)
    => (m (a, s) -> Either e (b, s))
    -> StateT s m a
    -> StateT s n (Either e b)
eitherMapStateT f p = state $ \s ->
    case f (runStateT p s) of
        Left e -> (Left e, s)
        Right (y, s') -> (Right y, s')

runFootnoteWriterT
    :: FootnoteWriterT m a
    -> [FootnoteType]
    -> m (a, FootnoteMap)
runFootnoteWriterT (FootnoteWriterT p) ts =
    runStateT (runReaderT p ts) Map.empty

footnoteP :: FootnoteParser ()
footnoteP = do
    o <- getOffset
    FootnoteWriterT ask >>= lift . choice . fmap footnoteP' >>= uncurry (add o)
  where
    add :: Int -> Label -> Footnote -> FootnoteParser ()
    add offset label fn = do
        fnMap <- FootnoteWriterT get
        case insert' label fn fnMap of
            Nothing ->
                fail' $
                    "Footnote {"
                        ++ unpack (unLabel label)
                        ++ "} already defined."
            Just fnMap' -> FootnoteWriterT $ put fnMap'
      where
        fail' = parseError . FancyError offset . Set.singleton . ErrorFail

footnoteP' :: FootnoteType -> Parser (Label, Footnote)
footnoteP' (FootnoteType kw fmt tt) =
    fmap (Footnote fmt) <$> hangingTextP' kw tt
