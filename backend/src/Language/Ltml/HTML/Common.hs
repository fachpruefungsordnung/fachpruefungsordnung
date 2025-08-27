{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Ltml.HTML.Common
    ( HtmlReaderState
    , GlobalState (..)
    , ReaderState (..)
    , initGlobalState
    , initReaderState
    , incSectionID
    , incSuperSectionID
    , FootnoteMap
    , convertLabelMap
    , FootnoteSet
    , NumLabel (..)
    , ToC
    , addTocEntry
    , EnumStyleMap
    , Delayed (..)
    , evalDelayed
    , returnNow
    ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State, get, modify)
import Data.DList (DList, snoc)
import qualified Data.DList as DList (empty)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import Data.Set (Set)
import qualified Data.Set as Set (empty)
import Data.Text (Text, pack)
import Language.Lsd.AST.Format
import Language.Lsd.AST.Type.Enum (EnumFormat)
import Language.Ltml.AST.Footnote (Footnote)
import Language.Ltml.AST.Label (Label (unLabel))
import Language.Ltml.HTML.Util (anchorLink)
import Lucid (Html)

-- | The Reader Monad is used for local tracking (e.g. enumNestingLevel).
--   The State Monad is used for global tracking (e.g. sectionIDs).
--   The Delayed type is used for delaying the actual lookup of references in the GlobalState.
--   This allows forward references, because at first a delayed object is build,
--   which is then evaluated aterwards with the final GlobalState.
type HtmlReaderState =
    ReaderT ReaderState (State GlobalState) (Delayed (Html ()))

data GlobalState = GlobalState
    { hasFlagged :: Bool
    -- ^ Is set True at every (Flagged ...); used to determine if current Flagged
    --   has any Flagged children
    , currentAppendixSectionID :: Int
    -- ^ Tracks the current appendix section id
    , currentSuperSectionID :: Int
    -- ^ Tracks the current super-section number
    , currentSectionID :: Int
    -- ^ Tracks the current section number
    , currentParagraphID :: Int
    -- ^ Tracks the current paragraph number in the current section
    , currentSentenceID :: Int
    -- ^ Tracks the current sentence number in the current paragraph
    , currentEnumItemID :: Int
    -- ^ Tracks the current enum item number in the current enumeration
    , currentFootnoteID :: Int
    -- ^ Tracks the id of the next footnote
    , usedFootnoteMap :: FootnoteMap
    -- ^ Maps all used footnotes labels to their id as an Int as Html
    --   and their text as Delayed Html, since they can also include references;
    --   This map is build during rendering with entries from "footnoteMap"
    --   from ReaderState and an additional id;
    --   Note: This map is document-scoped. Thus, it is reset when entering
    --         the next document.
    , locallyUsedFootnotes :: FootnoteSet
    -- ^ Holds a set of all footnotes that were used in the current section.
    --   When leaving a section, this is reset to the intial value.
    --   It is used to collect footnotes that should be rendered at the end
    --   of the current section (in ascending order of their footnote id).
    , labels :: [(Label, Html ())]
    -- ^ Holds all labels and the Html element that should be displayed when this label is referenced
    , labelWrapperFunc :: Label -> Html () -> Html ()
    -- ^ Wrapper around the Reference Html inside the TextTree (e.g. for adding anchor links)
    , tableOfContents :: ToC
    -- ^ Holds all entries for the table of contents as (key (e.g. § 1), title, HTML id as anchor link).
    , mangledLabelName :: Text
    -- ^ Mangled prefix name for generating new label names that do not exist in source language
    , mangledLabelID :: Int
    -- ^ Mangled postfix ID which is incremented and added to mangledLabelName to create unique htmlID
    , enumStyles :: EnumStyleMap
    -- ^ Maps EnumFormats to their css class name which implements the fitting Counter
    , mangledEnumCounterName :: Text
    -- ^ Holds prefix for generating new css class names for enum counter styles
    , mangledEnumCounterID :: Int
    -- ^ Holds postfix id which makes enum counter class name unique
    }

data ReaderState = ReaderState
    { shouldRender :: Bool
    -- ^ Is set True at every (Flagged True ...); to tell child Flagged to render
    , currentAppendixElementID :: Int
    -- ^ Tracks the current appendix element (document) id;
    --   This is in ReaderState, since it is controlled from the AppendixSection
    , appendixElementIdFormat :: IdentifierFormat
    -- ^ Tracks the identifier format of the current appendix element (document)
    , appendixElementTocKeyFormat :: TocKeyFormat
    -- ^ Tracks the toc key format of the current appendix element (document)
    , appendixElementMLabel :: Maybe Label
    -- ^ Holds the Maybe Label of the current (Node Document) in appendices;
    --   Used as a jump id inside the Document Heading
    , documentHeadingFormat :: Either (HeadingFormat False) (HeadingFormat True)
    -- ^ Holds format for current document heading
    --   (comes from DocumentContainer or AppendixSection)
    , isSingleParagraph :: Bool
    -- ^ Signals the child paragraph that it is the only child and thus should
    --   not have an visible identifier
    , currentEnumIDFormatString :: IdentifierFormat
    -- ^ Holds the FormatString that describes how the current enum item shoud
    --   be referenced
    , footnoteMap :: Map Label Footnote
    -- ^ Holds a map of all footnotes in the current document
    --   This is generated by the parser; some footnotes in this map
    --   might never be rendered (if they are not referenced from any section)
    }

initGlobalState :: GlobalState
initGlobalState =
    GlobalState
        { hasFlagged = False
        , currentAppendixSectionID = 1
        , currentSuperSectionID = 1
        , currentSectionID = 1
        , currentParagraphID = 1
        , currentSentenceID = 0
        , currentEnumItemID = 1
        , currentFootnoteID = 1
        , usedFootnoteMap = []
        , locallyUsedFootnotes = Set.empty
        , labels = []
        , -- \| Default rendering method is "preview", so no anchor links
          labelWrapperFunc = const id -- anchorLink
        , tableOfContents = DList.empty
        , mangledLabelName = "_TOC_ENTRY_"
        , mangledLabelID = 0
        , enumStyles = []
        , mangledEnumCounterName = "_ENUM_STYLE_"
        , mangledEnumCounterID = 0
        }

initReaderState :: ReaderState
initReaderState =
    ReaderState
        { shouldRender = False
        , currentAppendixElementID = 1
        , appendixElementIdFormat = error "Undefined appendix element id format!"
        , appendixElementTocKeyFormat = error "Undefined appendix element ToC format!"
        , appendixElementMLabel = Nothing
        , documentHeadingFormat = error "Undefined HeadingFormat!"
        , isSingleParagraph = False
        , currentEnumIDFormatString = error "Undefined enum id format!"
        , footnoteMap = Map.empty
        }

-------------------------------------------------------------------------------

-- | Increments currentSectionID in GlobalState
incSectionID :: ReaderT r (State GlobalState) ()
incSectionID = modify (\s -> s {currentSectionID = currentSectionID s + 1})

-- | Increments currentSuperSectionID in GlobalState
incSuperSectionID :: ReaderT r (State GlobalState) ()
incSuperSectionID = modify (\s -> s {currentSuperSectionID = currentSuperSectionID s + 1})

-------------------------------------------------------------------------------

-- | Maps Label to (ID, Text) as int and (delayed) html
type FootnoteMap = [(Label, (Int, Html (), Delayed (Html ())))]

-- | Converts FootnoteMap to Label Map used for "normal" references
convertLabelMap :: FootnoteMap -> [(Label, Html ())]
convertLabelMap = map (\(label, (_, idHtml, _)) -> (label, idHtml))

-------------------------------------------------------------------------------

-- | Set of footnote labels with their respective footnote id
type FootnoteSet = Set NumLabel

-- | Used for sorted insertion into the set of footnotes;
--   The Labels must be sorted by their footnote id
newtype NumLabel = NumLabel {unNumLabel :: (Int, Label)}

instance Eq NumLabel where
    (NumLabel (a, _)) == (NumLabel (b, _)) = a == b

instance Ord NumLabel where
    compare (NumLabel (a, _)) (NumLabel (b, _)) = compare a b

-------------------------------------------------------------------------------

-- | The ToC uses a difference list to get constant time appending at the end, which has no speed draw backs,
--   since the list is evaluated only ones when building the ToC Html at the end of rendering.
type ToC = DList (Html (), Delayed (Html ()), Text)

-- | Add entry to table of contents with: key Html (e.g. § 1), title Html and html anchor link id;
--   If Label is present uses it as the anchor link id, otherwise it creates a new mangled label name;
--   the used label name is returned;
addTocEntry
    :: Html ()
    -> Delayed (Html ())
    -> Maybe Label
    -> ReaderT ReaderState (State GlobalState) Text
addTocEntry key title mLabel = do
    globalState <- get
    htmlId <- case mLabel of
        -- \| Create new mangled name for non existing label
        Nothing ->
            -- \| Build mangled name by appending unique id to mangled label name
            let mangledLabel = mangledLabelName globalState <> pack (show (mangledLabelID globalState))
             in do
                    -- \| Increment mangled label id for next mangled label
                    modify (\s -> s {mangledLabelID = mangledLabelID s + 1})
                    return mangledLabel
        Just label -> return $ unLabel label
    modify
        (\s -> s {tableOfContents = snoc (tableOfContents s) (key, title, htmlId)})
    return htmlId

-------------------------------------------------------------------------------

-- | Maps EnumFormat to css class name which implements the counter:
--   Is used for reusing already existing classes, if the same EnumFormat occurs again
type EnumStyleMap = [(EnumFormat, Text)]

-------------------------------------------------------------------------------

data Delayed a = Now a | Later (GlobalState -> a)

evalDelayed :: Delayed a -> GlobalState -> a
evalDelayed (Now a) _ = a
evalDelayed (Later fa) s = fa s

returnNow :: Html () -> HtmlReaderState
returnNow = return . Now

instance (Monoid a) => Monoid (Delayed a) where
    mempty = Now mempty

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
