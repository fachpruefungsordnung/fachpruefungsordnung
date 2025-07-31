{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}
module Language.Ltml.HTML.Common
    ( HtmlReaderState
    , GlobalState (..)
    , ReaderState (..)
    , initGlobalState
    , initReaderState
    , Delayed (..)
    , evalDelayed
    , returnNow
    , fromNow
    ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Data.Text (Text)
import Lucid (Html)

-- | The Reader Monad is used for local tracking (e.g. enumNestingLevel).
--   The State Monad is used for global tracking (e.g. sectionIDs).
--   The Delayed type is used for delaying the actual lookup of references in the GlobalState.
--   This allows forward references, because at first a big delayed object is build,
--   which is then evaluated aterwards with the final GlobalState.
type HtmlReaderState =
    ReaderT ReaderState (State GlobalState) (Delayed (Html ()))

data GlobalState = GlobalState
    { currentSectionID :: Int
    -- ^ tracks the current section numbering
    , currentParagraphID :: Int
    -- ^ tracks the current paragraph number in the current section
    , currentSentenceID :: Int
    -- ^ tracks the current sentence number in the current paragraph
    , labels :: [(Text, Html ())]
    }

data ReaderState = ReaderState
    { enumNestingLevel :: Int
    -- ^ Tracks the current enumeration nesting level
    , currentSectionIDHtml :: Html ()
    -- ^ Holds the actual Html numbering that should be displayed for the current section
    , mCurrentParagraphIDHtml :: Maybe (Html ())
    -- ^ Holds the actual textual numbering that should be displayed when the current paragraph is referenced.
    --   Therefore this only holds the raw identifier and does not contain any extra symbols like ")" or ".".
    --   This is a Maybe type because the FormatString may not contain any IdentifierPlaceholder.
    --   In this case this will be Nothing.
    , isSingleParagraph :: Bool
    -- ^ signals the child paragraph that it is the only child and thus should not have an visible identifier
    }

initGlobalState :: GlobalState
initGlobalState =
    GlobalState
        { currentSectionID = 1
        , currentParagraphID = 1
        , currentSentenceID = 0
        , labels = []
        }

initReaderState :: ReaderState
initReaderState =
    ReaderState
        { enumNestingLevel = 0
        , currentSectionIDHtml = mempty
        , mCurrentParagraphIDHtml = Nothing
        , isSingleParagraph = False
        }

-------------------------------------------------------------------------------

data Delayed a = Now a | Later (GlobalState -> a)

evalDelayed :: Delayed a -> GlobalState -> a
evalDelayed (Now a) _ = a
evalDelayed (Later fa) s = fa s

returnNow :: Html () -> HtmlReaderState
returnNow = return . Now

fromNow :: Delayed a -> a
fromNow (Now a) = a
fromNow (Later _) = error "fromNow was called with Later"

instance (Semigroup a) => Semigroup (Delayed a) where
    Now a <> Now b = Now (a <> b)
    Now a <> Later fb = Later (\s -> a <> fb s)
    Later fa <> Now b = Later (\s -> fa s <> b)
    Later fa <> Later fb = Later (\s -> fa s <> fb s)

instance Functor Delayed where
    fmap f (Now a) = Now $ f a
    fmap f (Later fa) = Later (f . fa)

instance Applicative Delayed where
    pure = Now

    Now fa <*> Now a = Now (fa a)
    Now fa <*> Later fsa = Later (fa . fsa)
    Later fsfa <*> Now a = Later (\s -> fsfa s a)
    Later fsfa <*> Later fsa = Later (\s -> fsfa s (fsa s))

instance Monad Delayed where
    return = pure

    Now a >>= fa = fa a
    Later fsa >>= fa =
        Later
            ( \s -> case fa $ fsa s of
                Now b -> b
                Later fsb -> fsb s
            )