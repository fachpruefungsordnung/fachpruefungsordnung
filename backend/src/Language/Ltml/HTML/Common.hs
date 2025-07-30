module Language.Ltml.HTML.Common
    ( HtmlReaderState
    , GlobalState (..)
    , ReaderState (..)
    , initGlobalState
    , initReaderState
    ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Data.Text (Text)
import Lucid (Html)

type HtmlReaderState = ReaderT ReaderState (State GlobalState) (Html ())

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
        }
