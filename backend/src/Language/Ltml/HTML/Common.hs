{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Language.Ltml.HTML.Common
    ( HtmlReaderState
    , ReaderStateMonad
    , runReaderState
    , GlobalState (..)
    , ReaderState (..)
    , initGlobalState
    , initReaderState
    , incSectionID
    , incSuperSectionID
    , FootnoteMap
    , convertLabelMap
    , addUsedFootnoteLabels
    , FootnoteSet
    , NumLabel (..)
    , ToC
    , TocEntry
    , TocCategory (..)
    , addTocEntry
    , addPhantomTocEntry
    , PhantomTocEntry
    , RenderedTocEntry
    , Result (..)
    , result
    , EnumStyleMap
    , LabelWrapper
    , TocEntryWrapper
    , anchorLink
    , pageLink
    , mainPageAnchorLink
    , collectExportSection
    , exportLink
    , setHasErrors
    , Delayed (..)
    , evalDelayed
    , returnNow
    ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (State, get, modify, runState)
import Data.ByteString.Lazy (ByteString)
import Data.DList (DList, snoc)
import qualified Data.DList as DList (empty)
import Data.Map (Map)
import qualified Data.Map as Map (empty)
import Data.Set (Set)
import qualified Data.Set as Set (empty)
import Data.Text (Text, cons, pack)
import Language.Lsd.AST.Common (Fallback, NavTocHeading)
import Language.Lsd.AST.Format
import Language.Lsd.AST.Type.Enum (EnumFormat)
import Language.Lsd.AST.Type.Section (SectionFormat)
import Language.Ltml.AST.Footnote (Footnote)
import Language.Ltml.AST.Label (Label (unLabel))
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Language.Ltml.HTML.CSS.Util (cssClass_, (<#>))
import Lucid (Html, a_, div_, href_, span_, toHtml)

-- TODO: Third ConfigState? With custom Reader Monad that is read only

-- | The Reader Monad is used for local tracking (e.g. enumNestingLevel).
--   The State Monad is used for global tracking (e.g. sectionIDs).
--   The Delayed type is used for delaying the actual lookup of references in the GlobalState.
--   This allows forward references, because at first a delayed object is build,
--   which is then evaluated aterwards with the final GlobalState.
type HtmlReaderState = ReaderStateMonad (Delayed (Html ()))

type ReaderStateMonad a = ReaderT ReaderState (State GlobalState) a

runReaderState
    :: ReaderStateMonad a -> ReaderState -> GlobalState -> (a, GlobalState)
runReaderState ma readerState = runState (runReaderT ma readerState)

-------------------------------------------------------------------------------

data GlobalState = GlobalState
    { hasFlagged :: Bool
    -- ^ Is set True at every (Flagged ...); used to determine if current Flagged
    --   has any Flagged children
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
    , tableOfContents :: ToC
    -- ^ Holds all entries for the table of contents as (Maybe key (e.g. § 1),
    --   title, HTML id as anchor link, category). The title is wrapped into 'Result'.
    --   In case of an parse error this title will be set to an Error title.
    --   The 'Left' constructor holds metadata for the Frontend,
    --   which is ignored when rendering the 'ToC'.
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
    , exportSections :: [(Text, Delayed Text, Delayed (Html ()))]
    -- ^ Collects all (non-super) sections as their their @htmlID@, their 'Html' and their title
    , mainDocumentTitleHtml :: Delayed (Html ())
    -- ^ Styled title of the main Document for building exported sections
    , mainDocumentTitle :: Delayed Text
    -- ^ Raw title of the main Document for building HTML headers
    , hasErrors :: Bool
    -- ^ True if any error occured while parsing;
    --   Note: "soft" errors like undefined labels are not catched
    }

data ReaderState = ReaderState
    { shouldRender :: Bool
    -- ^ Is set True at every (Flagged True ...); to tell child Flagged to render
    , hasGlobalToC :: Bool
    -- ^ Signals if a Document should have a global or local ToC (if it has any);
    --   Is set by the DocumentContainer
    , appendixHasGlobalToC :: Bool
    -- ^ Should Documents inside of an AppendixSection should have a global ToC?
    --   This also means all Headings of those Documents appear in the global ToC
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
    , documentFallbackTitle :: Fallback NavTocHeading
    -- ^ Holds a fallback ToC title to send to the Frontend, if parsing the main
    --   Document failes. This is set by the DocumentContainer.
    , localSectionFormat :: SectionFormat
    -- ^ Defines the local 'SectionFormat'; is set by the 'SectionFormatted' wrapper
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
    , labelWrapperFunc :: LabelWrapper
    -- ^ Wrapper around the Reference Html inside the TextTree (e.g. for adding anchor links)
    , footnoteWrapperFunc :: LabelWrapper
    -- ^ Wrapper around Footnote reference Html inside the TextTree (e.g. for adding anchor links)
    , tocEntryWrapperFunc :: TocEntryWrapper
    -- ^ Wrapper around an ToC entry (e.g. for adding anchor links)
    , tocButtonWrapperFunc :: TocEntryWrapper
    -- ^ Wrapper around the button in the right column of the ToC (e.g. for adding page links)
    , exportLinkWrapper :: LabelWrapper
    -- ^ Wrapper around the ID of a section at the end of it (e.g. for adding export links)
    }

initGlobalState :: GlobalState
initGlobalState =
    GlobalState
        { hasFlagged = False
        , currentSuperSectionID = 1
        , currentSectionID = 1
        , currentParagraphID = 1
        , currentSentenceID = 0
        , currentEnumItemID = 1
        , currentFootnoteID = 1
        , usedFootnoteMap = []
        , locallyUsedFootnotes = Set.empty
        , labels = []
        , tableOfContents = DList.empty
        , mangledLabelName = "_TOC_ENTRY_"
        , mangledLabelID = 0
        , enumStyles = []
        , mangledEnumCounterName = "_ENUM_STYLE_"
        , mangledEnumCounterID = 0
        , exportSections = []
        , mainDocumentTitleHtml = mempty
        , mainDocumentTitle = mempty
        , hasErrors = False
        }

initReaderState :: ReaderState
initReaderState =
    ReaderState
        { shouldRender = True
        , hasGlobalToC = False
        , appendixHasGlobalToC = False
        , currentAppendixElementID = 1
        , appendixElementIdFormat = error "Undefined appendix element id format!"
        , appendixElementTocKeyFormat = error "Undefined appendix element ToC format!"
        , appendixElementMLabel = Nothing
        , documentHeadingFormat = error "Undefined HeadingFormat!"
        , documentFallbackTitle = error "Undefined Main Document Fallback Heading!"
        , localSectionFormat = error "Undefined SectionFormat!"
        , isSingleParagraph = False
        , currentEnumIDFormatString = error "Undefined enum id format!"
        , footnoteMap = Map.empty
        , -- \| Default rendering method is "preview", so no anchor links
          --    and no export links at all
          labelWrapperFunc = const id -- anchorLink
        , footnoteWrapperFunc = const id
        , tocEntryWrapperFunc = const $ const id
        , tocButtonWrapperFunc = const anchorLink
        , exportLinkWrapper = const mempty
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

-- | Adds used Footnotes as "normal" references to GlobalState
--   Note: Footnote Labels will shadow normal Labels with the same name!
addUsedFootnoteLabels :: GlobalState -> GlobalState
addUsedFootnoteLabels globalState =
    let usedFootnotes = convertLabelMap $ usedFootnoteMap globalState
     in globalState {labels = usedFootnotes ++ labels globalState}

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
type ToC = DList (Either PhantomTocEntry TocEntry)

type TocEntry = (Maybe (Html ()), Result (Delayed (Html ())), Text, TocCategory)

data TocCategory = SomeSection | Other

-- | Toc Entry that only exists to send info to the Frontend;
--   It is ignored when rendering a ToC
type PhantomTocEntry = (Maybe (Html ()), Result (Html ()))

-- | Add entry to table of contents with: key Html (e.g. § 1), title Html and html anchor link id;
--   If Label is present uses it as the anchor link id, otherwise it creates a new mangled label name;
--   the used label name is returned;
addTocEntry
    :: Maybe (Html ())
    -> Result (Delayed (Html ()))
    -> Maybe Label
    -> TocCategory
    -> ReaderStateMonad Text
addTocEntry mKey title mLabel cat = do
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
        ( \s ->
            s
                { tableOfContents = snoc (tableOfContents s) (Right (mKey, title, htmlId, cat))
                }
        )
    return htmlId

-- | Adds a phantom entry into the table of contents, which is ignored when rendering.
--   Its only purpose is to tell the frontend if a parse error occured in this segment.
--   This is only meant to be used for segments that do not have a normal Toc entry,
--   like @SimpleSection@s.
addPhantomTocEntry
    :: Result (Html ()) -> ReaderStateMonad ()
addPhantomTocEntry resHtml =
    -- \| Phantom Entries do not have an ID and are not Delayed
    let tocEntry = (Nothing, resHtml)
     in modify (\s -> s {tableOfContents = snoc (tableOfContents s) (Left tocEntry)})

-- | Type of exported ToC Entries (especially for Frontend);
--   @Result@ signals if the generated title was parsed successfully or not
type RenderedTocEntry = (Maybe ByteString, Result ByteString)

data Result a = Success a | Error a
    deriving (Show)

-- | Takes a @Result a@, a success function and an error function.
--   Applies one of the two functions depending on the 'Result' constructor.
result :: (a -> b) -> (a -> b) -> Result a -> b
result fSuc fErr res = case res of
    Success a -> fSuc a
    Error a -> fErr a

instance Functor Result where
    fmap f (Success a) = Success (f a)
    fmap f (Error a) = Error (f a)

-------------------------------------------------------------------------------

-- | Maps EnumFormat to css class name which implements the counter:
--   Is used for reusing already existing classes, if the same EnumFormat occurs again
type EnumStyleMap = [(EnumFormat, Text)]

-------------------------------------------------------------------------------

-- | Type of function thats used to wrap Label information around Html;
--   e.g. for adding anchor links
type LabelWrapper = Label -> Html () -> Html ()

type TocEntryWrapper = TocCategory -> LabelWrapper

-- | Converts 'Label' into @<a href = "#<label>">@ for jumping to a HTML id
anchorLink :: LabelWrapper
anchorLink label = a_ [cssClass_ Class.AnchorLink, href_ (cons '#' $ unLabel label)]

-- | Converts 'Label' into @<a href = "<path>/<label>.html">@ for jumping
--   to another page
pageLink
    :: FilePath
    -- ^ Path prefix
    -> TocEntryWrapper
pageLink _ Other _ = const mempty
pageLink path _ label =
    a_ [cssClass_ Class.ButtonLink, href_ (labelPath path label)]

-- | Converts 'Label' into @<a href = "<path>#<label>">@ for jumping
--   to another pages anchor
mainPageAnchorLink :: FilePath -> LabelWrapper
mainPageAnchorLink path label = a_ [cssClass_ Class.AnchorLink, href_ (pack path <> "#" <> unLabel label)]

-- | Builds a link with 'labelPath', prefix "Zur Einzelansicht"
--   and adds some vertical spacing
exportLink :: FilePath -> LabelWrapper
exportLink path label =
    div_
        . a_ [cssClass_ Class.AnchorLink, href_ (labelPath path label)]
        . ((span_ <#> Class.LargeFontSize $ toHtml ("↗ " :: Text)) <>)

-- | Builds "<path>/<label>.html"
labelPath :: FilePath -> Label -> Text
labelPath path label = pack path <> "/" <> unLabel label <> ".html"

-------------------------------------------------------------------------------

-- | Adds a 'Section' with @htmlId@ and HTML @title@ to 'GlobalState'
collectExportSection
    :: Text -> Delayed Text -> Delayed (Html ()) -> ReaderStateMonad ()
collectExportSection htmlId title sectionHtml =
    modify
        (\s -> s {exportSections = (htmlId, title, sectionHtml) : exportSections s})

-- | Sets the 'hasErrors' flag to @True@
setHasErrors :: ReaderStateMonad ()
setHasErrors = modify (\s -> s {hasErrors = True})

-------------------------------------------------------------------------------

data Delayed a = Now a | Later (GlobalState -> a)

evalDelayed :: GlobalState -> Delayed a -> a
evalDelayed _ (Now a) = a
evalDelayed s (Later fa) = fa s

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
