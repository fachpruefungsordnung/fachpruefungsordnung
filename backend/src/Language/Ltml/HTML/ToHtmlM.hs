{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.ToHtmlM (toHtmlM) where

import Control.Monad (join, zipWithM)
import Control.Monad.Reader
import Control.Monad.State
import Data.DList (toList)
import Data.Either (rights)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import Data.Void (Void, absurd)
import Language.Lsd.AST.Common (Fallback (..), NavTocHeading (..))
import Language.Lsd.AST.Format (HeadingFormat)
import Language.Lsd.AST.Type.AppendixSection
    ( AppendixElementFormat (..)
    , AppendixSectionFormat (..)
    , AppendixSectionTitle (..)
    )
import Language.Lsd.AST.Type.Document
    ( DocumentFormat (..)
    , TocFormat (..)
    , TocHeading (..)
    )
import Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerFormat (..)
    , MainDocumentFormat (..)
    )
import Language.Lsd.AST.Type.Enum (EnumFormat (..), EnumItemFormat (..))
import Language.Lsd.AST.Type.Footnote
    ( FootnoteFormat (SuperscriptFootnoteFormat)
    )
import Language.Lsd.AST.Type.Section (SectionFormatted (..))
import Language.Lsd.AST.Type.SimpleParagraph (SimpleParagraphFormat (..))
import Language.Lsd.AST.Type.SimpleSection (SimpleSectionFormat (..))
import Language.Ltml.AST.AppendixSection (AppendixSection (..))
import Language.Ltml.AST.Document
import Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (..)
    , DocumentContainerHeader (..)
    )
import Language.Ltml.AST.Footnote (Footnote (..))
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Module
    ( Attribute (..)
    , Category (..)
    , Module (..)
    , ModuleBlock (..)
    , ModuleSchema (..)
    , ModuleTable (..)
    )
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.AST.Paragraph (Paragraph (..))
import Language.Ltml.AST.Section
import Language.Ltml.AST.SimpleBlock (SimpleBlock (..))
import Language.Ltml.AST.SimpleParagraph (SimpleParagraph (..))
import Language.Ltml.AST.SimpleSection (SimpleSection (..))
import Language.Ltml.AST.Table (Cell (..), Row (..), Table (..))
import Language.Ltml.AST.Text
import Language.Ltml.Common (Flagged (..), Flagged', NavTocHeaded (..), Parsed)
import Language.Ltml.HTML.CSS.Classes (ToCssClass (toCssClass))
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Language.Ltml.HTML.CSS.Util
import Language.Ltml.HTML.Common
import Language.Ltml.HTML.FormatString
import Language.Ltml.HTML.References
import Language.Ltml.HTML.Util
import Lucid
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty)

-- | Monadic Class to render @a@ to @Delayed (Html ())@
--   using a @Reader@ and a @State@ Monad
class ToHtmlM a where
    toHtmlM :: a -> HtmlReaderState

instance ToHtmlM DocumentContainer where
    toHtmlM
        ( DocumentContainer
                ( DocumentContainerFormat
                        _
                        _
                        (MainDocumentFormat fallbackDocHeading docHeadingFormat)
                    )
                navTocParsedHeader
                doc
                appendices
            ) = do
            modify (\s -> s {documentFallbackTitle = fallbackDocHeading})
            -- \| Header is rendered to mempty, but this might generate an error box
            headerHtml <- toHtmlM navTocParsedHeader
            -- \| Main Document has a global ToC, appendices typically do not
            mainDocHtml <-
                local
                    ( \s ->
                        s
                            { hasGlobalToC = True
                            , documentHeadingFormat = Left docHeadingFormat
                            }
                    )
                    $ toHtmlM doc
            readerState <- ask
            appendicesHtml <-
                local (\s -> s {hasGlobalToC = appendixHasGlobalToC readerState}) $
                    toHtmlM appendices
            -- Render Header only if whole DocumentContainer should be rendered
            render <- asks shouldRender
            return $
                (if render then headerHtml else mempty)
                    <> mainDocHtml
                    <> appendicesHtml

-- | Ignore Header since it only defines PDF related stuff
instance ToHtmlM DocumentContainerHeader where
    toHtmlM _ = returnNow mempty

-- | This instance is used for documents inside the appendix,
--   since the main document does not have a label.
instance ToHtmlM (Node Document) where
    toHtmlM (Node mLabel doc) = do
        appendixDocID <- asks currentAppendixElementID
        addMaybeLabelToState mLabel (toHtml $ show appendixDocID)
        local (\s -> s {appendixElementMLabel = mLabel}) $ toHtmlM doc

instance ToHtmlM Document where
    -- \| builds Lucid 2 HTML from a Ltml Document AST
    toHtmlM
        ( Document
                (DocumentFormat mTocFormat)
                documentHeading
                (DocumentBody introSSections sectionBody outroSSections)
                footNotes
            ) =
            local (\s -> s {footnoteMap = footNotes}) $ do
                modify
                    ( \s ->
                        s
                            { -- \| Reset used footnotes (footnotes are document-scoped)
                              usedFootnoteMap = usedFootnoteMap initGlobalState
                            , -- \| Reset footnote counter for this document
                              currentFootnoteID = currentFootnoteID initGlobalState
                            , -- \| Reset Section counters for this document
                              currentSectionID = currentSectionID initGlobalState
                            , currentSuperSectionID = currentSuperSectionID initGlobalState
                            }
                    )
                titleHtml <- toHtmlM documentHeading
                renderDoc <- asks shouldRender
                -- \| mTocFormat = Nothing, means that no ToC should be rendered
                let renderToC = isJust mTocFormat && renderDoc

                -- \| If current Document has a local ToC its Sections
                --   should NOT appear in global ToC, thats why we save the current global ToC
                hasGlobalToc <- asks hasGlobalToC
                (tocHtml, introHtml, mainHtml, outroHtml) <-
                    if hasGlobalToc
                        then do
                            -- \| Render ToC but as a Later to use the final GlobalState
                            delayedTocHtml <- renderDelayedToc mTocFormat
                            introHtml <- toHtmlM introSSections
                            mainHtml <- toHtmlM sectionBody
                            outroHtml <- toHtmlM outroSSections
                            return (delayedTocHtml, introHtml, mainHtml, outroHtml)
                        else
                            -- \| Reset ToC temporarily to build a local ToC, then write back the global ToC
                            withModified
                                tableOfContents
                                (\s a -> s {tableOfContents = a})
                                (tableOfContents initGlobalState)
                                $ do
                                    introHtml <- toHtmlM introSSections
                                    mainHtml <- toHtmlM sectionBody
                                    outroHtml <- toHtmlM outroSSections
                                    -- \| Render ToC last so local ToC has all Headings set,
                                    --    since the local ToC uses the current State
                                    localTocHtml <- renderLocalToc mTocFormat
                                    return (localTocHtml, introHtml, mainHtml, outroHtml)

                -- \| Render DocumentHeading / ToC only if renderFlag was set by parent
                return $
                    div_ <#> Class.Document
                        <$> ( (if renderDoc then titleHtml else mempty)
                                <> introHtml
                                <> (if renderToC then tocHtml else mempty)
                                <> mainHtml
                                <> outroHtml
                            )

-- | Does not only produce the default error box on error,
--   but also handles ToC entries.
instance ToHtmlM (Parsed DocumentHeading) where
    toHtmlM eErrDocumentHeading = do
        -- \| Title which is used if a parse error occurs
        fallbackTitle <- gets documentFallbackTitle
        (resType, titleHtml) <- case eErrDocumentHeading of
            Left _ -> do
                setHasErrors
                failedHeadingTextHtml <- toHtmlM fallbackTitle
                return (Error, failedHeadingTextHtml)
            Right (DocumentHeading headingTextTree) -> do
                headingTextHtml <- toHtmlM headingTextTree
                -- \| Used for setting HTML title
                modify (\s -> s {mainDocumentTitle = headingText headingTextTree})
                return (Success, headingTextHtml)
        -- \| Get HeadingFormat from DocumentContainer or AppendixSection
        headingFormatS <- asks documentHeadingFormat
        -- \| Here we check if we are inside an appendix, since
        --   the appendix heading format has an id and the main documents has not
        (mIdHtml, formattedTitle, mLabel) <- case headingFormatS of
            Left headFormat -> buildMainHeading titleHtml headFormat
            Right headFormatId -> buildAppendixHeading titleHtml headFormatId
        htmlId <- addTocEntry mIdHtml (resType titleHtml) mLabel Other
        -- \| In case of a parse error, output an error box
        return $
            either
                (Now . parseErrorHtml (Just htmlId))
                ( const $
                    h1_ [cssClass_ Class.DocumentTitle, cssClass_ Class.Anchor, id_ htmlId]
                        <$> formattedTitle
                )
                eErrDocumentHeading
      where
        -- \| Main Document Heading without Id and without Label
        --    This should only be called once
        buildMainHeading
            :: Delayed (Html ())
            -> HeadingFormat False
            -> ReaderStateMonad (Maybe (Html ()), Delayed (Html ()), Maybe Label)
        buildMainHeading dTitleHtml headFormat = do
            let formattedTitle = headingFormat headFormat <$> dTitleHtml
            -- \| Used for adding title to exported sections
            modify (\s -> s {mainDocumentTitleHtml = formattedTitle})
            return (Nothing, formattedTitle, Nothing)

        -- \| Appendix Docuemnt Heading with Id and Label
        buildAppendixHeading
            :: Delayed (Html ())
            -> HeadingFormat True
            -> ReaderStateMonad (Maybe (Html ()), Delayed (Html ()), Maybe Label)
        buildAppendixHeading dTitleHtml headFormatId = do
            -- \| Heading for Appendix Element (with id and toc key)
            docId <- asks currentAppendixElementID
            idFormat <- asks appendixElementIdFormat
            tocFormat <- asks appendixElementTocKeyFormat
            let (headingHtml, tocHtml) = appendixFormat idFormat docId tocFormat headFormatId dTitleHtml
            -- \| Check if current document has Label and build ToC entry
            mLabel <- asks appendixElementMLabel
            return (Just tocHtml, headingHtml, mLabel)

-------------------------------------------------------------------------------

-- | This combined instances creates the sectionIDHtml before building the reference,
--   which is needed for correct referencing
instance ToHtmlM (Node Section) where
    toHtmlM
        ( Node
                mLabel
                ( Section
                        parsedHeading
                        sectionBody
                    )
            ) = do
            globalState <- get
            sectionFormatS <- asks localSectionFormat
            let (sectionIDGetter, incrementSectionID, sectionCssClass) =
                    -- \| Check if we are inside a section or a super-section
                    -- TODO: Is (SimpleLeafSectionBody [SimpleBlocks]) counted as super-section? (i think yes)
                    if isSuper sectionBody
                        then (currentSuperSectionID, incSuperSectionID, Class.SuperSection)
                        else (currentSectionID, incSectionID, Class.Section)
                (sectionIDHtml, sectionTocKeyHtml) = sectionFormat sectionFormatS (sectionIDGetter globalState)
             in do
                    addMaybeLabelToState mLabel sectionIDHtml
                    -- \| Render parsed Heading, which also creates ToC Entry
                    (headingHtml, tocId, rawTitle) <-
                        buildHeadingHtml sectionIDHtml mLabel sectionTocKeyHtml parsedHeading
                    headingState <- get
                    childrenHtml <- toHtmlM sectionBody
                    -- \| Also render footnotes in super-sections, since their heading
                    --    could contain footnoteRefs
                    childrensGlobalState <- get
                    let footNoteState = addUsedFootnotes childrensGlobalState headingState
                    footnotesHtml <- toHtmlM (locallyUsedFootnotes footNoteState)
                    -- \| Reset locally used set to inital value
                    modify (\s -> s {locallyUsedFootnotes = locallyUsedFootnotes initGlobalState})
                    -- \| increment (super)SectionID for next section
                    incrementSectionID

                    exportLinkFunc <- asks exportLinkWrapper
                    let rawdTitleHtml = toHtml . (" " <>) <$> rawTitle
                        exportLinkHtml =
                            exportLinkFunc (Label tocId) <$> (pure sectionTocKeyHtml <> rawdTitleHtml)

                        sectionHtml =
                            section_ [cssClass_ sectionCssClass]
                                <$> (headingHtml <> childrenHtml <> footnotesHtml <> exportLinkHtml)
                    -- \| Collects all sections for possible export
                    collectExportSection tocId rawTitle sectionHtml

                    return sectionHtml
          where
            -- \| Also adds table of contents entry for section
            buildHeadingHtml
                :: Html ()
                -> Maybe Label
                -> Html ()
                -> Parsed Heading
                -> ReaderStateMonad
                    ( Delayed (Html ())
                    , -- \^ Formatted title
                      Text
                    , -- \^ html id
                      Delayed Text
                      -- \^ raw textual title
                    )

            buildHeadingHtml sectionIDHtml mLabelH tocKeyHtml eErrHeading =
                case eErrHeading of
                    Left parseErr -> do
                        setHasErrors
                        htmlId <-
                            -- In case of a Heading failure
                            -- we simply display the ID as the title
                            createTocEntryH Nothing (Error $ Now tocKeyHtml)
                        return (Now $ parseErrorHtml (Just htmlId) parseErr, htmlId, mempty)
                    Right (Heading headingFormatS title) -> do
                        titleHtml <- toHtmlM title
                        let rawTitleText = headingText title
                        -- TODO: Maybe: Toc entry for section has no footnotes (headingText skips them),
                        --       since the could contain @<a>@. When the Toc entry is wrapped in another
                        --       @<a>@ it creates invalid HTML
                        htmlId <- createTocEntryH (Just tocKeyHtml) (Success $ toHtml <$> rawTitleText)
                        return
                            ( h2_ [cssClass_ Class.Heading, cssClass_ Class.Anchor, id_ htmlId]
                                . headingFormatId headingFormatS sectionIDHtml
                                <$> titleHtml
                            , htmlId
                            , rawTitleText
                            )
              where
                createTocEntryH mIdHtml rTitle = addTocEntry mIdHtml rTitle mLabelH SomeSection

instance ToHtmlM SectionBody where
    toHtmlM sectionBody = case sectionBody of
        -- \| Super Section
        -- \| We have to save the super-section counter, since super-sections are counted locally
        InnerSectionBody nodeSections -> do
            superSectionID <- gets currentSuperSectionID
            modify (\s -> s {currentSuperSectionID = currentSuperSectionID initGlobalState})
            bodyHtml <- toHtmlM nodeSections
            modify (\s -> s {currentSuperSectionID = superSectionID})
            return bodyHtml

        -- \| Section
        -- \| In this case the children are paragraphs, so we set the needed flag for them
        --    to decide if the should have a visible id
        LeafSectionBody nodeParagraphs -> do
            paragraphsHtml <-
                local (\s -> s {isSingleParagraph = length nodeParagraphs == 1}) $
                    toHtmlM nodeParagraphs
            -- \| reset paragraphID for next section
            modify (\s -> s {currentParagraphID = 1})
            return paragraphsHtml
        SimpleLeafSectionBody simpleBlocks -> toHtmlM simpleBlocks

-- | Combined instance since the paragraphIDHtml has to be build before the reference is generated
instance ToHtmlM (Node Paragraph) where
    toHtmlM (Node mLabel (Paragraph format textTrees)) = do
        globalState <- get
        let (paragraphIDHtml, paragraphKeyHtml) = paragraphFormat format (currentParagraphID globalState)
         in do
                addMaybeLabelToState mLabel paragraphIDHtml
                -- \| Group raw text (without enums) into <div> for flex layout spacing
                childText <- renderDivGrouped textTrees
                modify (\s -> s {currentParagraphID = currentParagraphID s + 1})
                -- \| Reset sentence id for next paragraph
                modify (\s -> s {currentSentenceID = 0})
                readerState <- ask
                return $
                    div_ [cssClass_ Class.Paragraph, cssClass_ Class.Anchor, mId_ mLabel]
                        -- \| If this is the only paragraph inside this section we drop the visible paragraphID
                        <$> let idHtml = if isSingleParagraph readerState then mempty else paragraphKeyHtml
                             in return (div_ <#> Class.ParagraphID $ idHtml)
                                    <> div_
                                        <#> Class.TextContainer
                                    <$> childText

-------------------------------------------------------------------------------

-- | Wrapper for block of SimpleParagraphs and table
instance ToHtmlM SimpleBlock where
    toHtmlM simpleBlock = case simpleBlock of
        SimpleParagraphBlock simpleParagraph -> toHtmlM simpleParagraph
        TableBlock table -> toHtmlM table
        ModuleSchemaBlock moduleBlock -> toHtmlM moduleBlock

-- | Section without Heading and Identifier
instance ToHtmlM SimpleSection where
    toHtmlM (SimpleSection (SimpleSectionFormat hasVBar) sParagraphs) = do
        -- \| Possibly add vertical bar at the beginning
        let pre = if hasVBar then div_ $ hr_ [] else mempty
        paragraphsHtml <- toHtmlM sParagraphs
        -- \| If <section> would be empty, skip it
        let renderSSection = hasVBar || not (null sParagraphs)
        return $
            if renderSSection
                then (section_ <#> Class.Section) <$> Now pre <> paragraphsHtml
                else mempty

-- | Paragraph without identifier
instance ToHtmlM SimpleParagraph where
    toHtmlM (SimpleParagraph (SimpleParagraphFormat typography) textTrees) = do
        childText <- toHtmlM textTrees
        return $
            div_
                (cssClass_ Class.TextContainer : cssClasses_ (Class.toCssClasses typography))
                <$> childText

-------------------------------------------------------------------------------

instance ToHtmlM Table where
    toHtmlM (Table rows) = do
        rowsHtml <- mconcat <$> mapM row rows
        return $ (table_ <#> Class.TableOfContents) . tbody_ <$> rowsHtml
      where
        row :: Row -> HtmlReaderState
        row (Row cells) = do
            cellsHtml <- mconcat <$> mapM cell cells
            return $ tr_ <$> cellsHtml

        cell :: Cell -> HtmlReaderState
        cell (Cell _ 0 0) = returnNow mempty
        cell (Cell text 1 1) = do
            textHtml <- toHtmlM text
            return $ td_ <$> textHtml
        cell (Cell text colspan rowspan) = do
            textHtml <- toHtmlM text
            return $ td_ [colspan_ (iToT colspan), rowspan_ (iToT rowspan)] <$> textHtml

instance ToHtmlM ModuleBlock where
    toHtmlM (ModuleBlock (ModuleSchema features) moduleTable) = do
        featureDHtmls <- mapM (toHtmlM . unAttribute) features

        rowDHtmls <- case moduleTable of
            Plain modules -> mapM moduleRow modules
            Categorized categories -> mapM categoryRows categories

        return $ do
            featureHtmls <- sequence featureDHtmls
            rowHtmls <- sequence rowDHtmls

            -- add single empty cell to thead for category offset
            let headerPlaceholder =
                    case moduleTable of
                        Plain _ -> mempty
                        Categorized _ -> th_ mempty
                colgroup = colgroup_ $ col_ <#> Class.MinSizeColumn
                thead = thead_ $ tr_ $ (headerPlaceholder <>) $ mconcat $ map th_ featureHtmls
                tbody = tbody_ $ mconcat rowHtmls
            Now $
                div_ <#> Class.TableContainer $
                    table_ <#> Class.Table $
                        colgroup <> thead <> tbody
      where
        categoryRows :: Category -> HtmlReaderState
        categoryRows (Category name modules) = do
            categoryHtml <- toHtmlM $ unAttribute name
            moduleDHtmls <- mapM moduleHtml modules

            return $ do
                moduleHtmls <- sequence moduleDHtmls
                cat <- categoryHtml
                let
                    -- TODO: use iToT here
                    categoryCell =
                        td_
                            [rowspan_ (pack . show . length $ modules), cssClass_ Class.TableCentered]
                            cat

                    categoryRow = case moduleHtmls of
                        [] -> [tr_ categoryCell]
                        (m : ms) -> map tr_ (categoryCell <> m : ms)
                return $ mconcat categoryRow

        moduleHtml :: Module -> HtmlReaderState
        moduleHtml (Module attr) = do
            attrDHtmls <- mapM (toHtmlM . unAttribute) attr
            return $ do
                attrHtmls <- sequence attrDHtmls
                return $ mconcat $ map (td_ <#> Class.TableCentered) attrHtmls

        moduleRow :: Module -> HtmlReaderState
        moduleRow m = do
            attrsHtml <- moduleHtml m
            return $ tr_ <$> attrsHtml

-------------------------------------------------------------------------------

instance
    (ToHtmlM fnref, ToCssClass style, ToHtmlM enum, ToHtmlM special)
    => ToHtmlM (TextTree lnbrk fnref style enum special)
    where
    toHtmlM textTree = case textTree of
        Word text -> returnNow $ toHtml text
        Space -> returnNow $ toHtml (" " :: Text)
        NonBreakingSpace -> returnNow $ toHtmlRaw ("&nbsp;" :: Text)
        -- \| ignore value since type Void does not have any values
        LineBreak _ -> returnNow $ br_ []
        Special special -> toHtmlM special
        Reference label -> do
            -- \| Label func for wrapping arbitrary Html (like anchor links) around the reference
            labelFunc <- asks labelWrapperFunc
            return $ Later $ \globalState ->
                case lookup label $ labels globalState of
                    -- \| Label was not found in GlobalState and a red error is emitted
                    Nothing -> htmlError ("Label \"" <> unLabel label <> "\" not found!")
                    Just labelHtml -> labelFunc label labelHtml
        Styled style textTrees ->
            let styleClass = toCssClass style
             in -- \| Wrap raw text in <span> and enums in <div>
                renderGroupedTextTree (span_ <#> styleClass) (div_ <#> styleClass) textTrees
        Enum enum -> toHtmlM enum
        FootnoteRef fnref -> toHtmlM fnref

-- | Increment sentence counter and add Label to GlobalState, if there is one
instance ToHtmlM SentenceStart where
    toHtmlM (SentenceStart mLabel) = do
        modify (\s -> s {currentSentenceID = currentSentenceID s + 1})
        globalState <- get
        -- \| Add Maybe Label with just the sentence number
        addMaybeLabelToState mLabel (toHtml $ show (currentSentenceID globalState))
        case mLabel of
            Nothing -> returnNow mempty
            Just label -> returnNow $ span_ [cssClass_ Class.Anchor, id_ (unLabel label)] mempty

instance ToHtmlM Enumeration where
    toHtmlM (Enumeration enumFormatS@(EnumFormat (EnumItemFormat idFormat _)) enumItems) = do
        -- \| Build enum format and add it to global state for creating the css classes later
        enumCounterClass <- enumFormat enumFormatS
        -- \| Reset enumItemID for this Enumeration
        modify (\s -> s {currentEnumItemID = 1})
        -- \| Render enum items with correct id format
        nested <-
            mapM
                (local (\s -> s {currentEnumIDFormatString = idFormat}) . toHtmlM)
                enumItems
        return $ do
            nestedHtml <- sequence nested
            let enumItemsHtml = foldr ((>>) . li_) (mempty :: Html ()) nestedHtml
             in return $
                    ol_ [cssClass_ Class.Enumeration, class_ enumCounterClass] enumItemsHtml

instance ToHtmlM (Node EnumItem) where
    toHtmlM (Node mLabel (EnumItem textTrees)) = do
        -- \| Save current enum item id, if nested enumerations follow and reset it
        enumItemID <- gets currentEnumItemID
        -- \| Build reference with EnumFormat from ReaderState
        enumItemRefHtml <- buildEnumItemRefHtml enumItemID
        addMaybeLabelToState mLabel enumItemRefHtml
        -- \| Render <div> grouped raw text (without enums) to get correct flex spacing
        enumItemHtml <- renderDivGrouped textTrees
        -- \| Increment enumItemID for next enumItem
        modify (\s -> s {currentEnumItemID = enumItemID + 1})
        return $
            div_ [cssClass_ Class.TextContainer, cssClass_ Class.Anchor, mId_ mLabel]
                <$> enumItemHtml

instance ToHtmlM FootnoteReference where
    toHtmlM (FootnoteReference label) = do
        usedFootnotes <- gets usedFootnoteMap
        -- \| Check if footnote was already referenced (document-scoped)
        let mFootnoteIdText = lookup label usedFootnotes
        case mFootnoteIdText of
            Just (footnoteID, footnoteIdHtml, _) -> createFootnoteRef footnoteIdHtml footnoteID label
            Nothing -> do
                -- \| Look for label in footnoteMap with unused footnotes
                unusedFootnoteMap <- asks footnoteMap
                case Map.lookup label unusedFootnoteMap of
                    -- \| Footnote Label does not exist
                    Nothing -> do
                        setHasErrors
                        returnNow $ htmlError $ "Footnote Label \"" <> unLabel label <> "\" not found!"
                    Just footnote -> do
                        footnoteID <- gets currentFootnoteID
                        let footnoteIdHtml = toHtml $ show footnoteID
                        footnoteTextHtml <- toHtmlM footnote
                        -- \| Add Label for normal references to this footnote
                        addMaybeLabelToState (Just label) footnoteIdHtml
                        -- \| Add new used footnote with id and rendered (delayed) text
                        modify
                            ( \s ->
                                s
                                    { usedFootnoteMap =
                                        (label, (footnoteID, footnoteIdHtml, footnoteTextHtml)) : usedFootnoteMap s
                                    , currentFootnoteID = currentFootnoteID s + 1
                                    }
                            )
                        createFootnoteRef footnoteIdHtml footnoteID label
      where
        -- \| Creates footnote reference html and adds footnote label to locally used footnotes
        createFootnoteRef :: Html () -> Int -> Label -> HtmlReaderState
        createFootnoteRef footHtml footId footLabel = do
            modify
                ( \s ->
                    s
                        { locallyUsedFootnotes =
                            Set.insert (NumLabel (footId, footLabel)) (locallyUsedFootnotes s)
                        }
                )
            -- \| Function for wrapping arbitrary Html (like anchor links) around footnote refs
            footnoteFunc <- asks footnoteWrapperFunc
            returnNow $ sup_ $ footnoteFunc footLabel footHtml

instance ToHtmlM Footnote where
    toHtmlM (Footnote SuperscriptFootnoteFormat textTrees) = do
        -- \| Grouped rendering is not necessary, since enums
        --   are not allowed inside of footnotes
        toHtmlM textTrees

instance ToHtmlM FootnoteSet where
    toHtmlM idLabelSet = do
        -- \| Get ascending (by footnoteId) list of Labels (drop id)
        let footnotes = map (snd . unNumLabel) $ Set.toAscList idLabelSet
        globalFootnoteMap <- gets usedFootnoteMap
        delayedFootnotesHtml <- mapM (toFootnoteHtml globalFootnoteMap) footnotes
        return $ do
            footnoteHtmls <- sequence delayedFootnotesHtml
            -- \| If there are no footnotes, dont output empty <div>
            if null footnoteHtmls
                then return mempty
                else do
                    let combinedFootnotesHtml = mconcat footnoteHtmls
                    -- \| Wrap all footnotes into one <div>
                    return $ div_ <#> Class.FootnoteContainer $ hr_ [] <> combinedFootnotesHtml
      where
        -- \| Lookup footnote label and build single footnote html
        toFootnoteHtml :: FootnoteMap -> Label -> HtmlReaderState
        toFootnoteHtml idTextMap label =
            case lookup label idTextMap of
                -- \| This should never happen (hopefully)
                Nothing ->
                    error
                        ( "footnote label \""
                            <> unpack (unLabel label)
                            <> "\" was used in current section, but never added to global used footnote map!"
                        )
                Just (_, idHtml, delayedTextHtml) ->
                    -- \| <div> <sup>id</sup> <span>text</span> </div>
                    return
                        ( ( div_ [cssClass_ Class.Footnote, cssClass_ Class.Anchor, id_ (unLabel label)]
                                <$> ((sup_ <#> Class.FootnoteID) idHtml <>)
                          )
                            . span_
                            <$> delayedTextHtml
                        )

-------------------------------------------------------------------------------

instance ToHtmlM AppendixSection where
    toHtmlM
        ( AppendixSection
                ( AppendixSectionFormat
                        (AppendixSectionTitle appendixSectionTitle)
                        (AppendixElementFormat idFormat tocFormat headFormat)
                    )
                nodeDocuments
            ) = do
            -- \| Empty appendices are dropped from HTML structure and ToC
            let isEmpty = null nodeDocuments
            -- \| Add Entry to ToC but without ID
            htmlId <-
                if isEmpty
                    then
                        -- \| If appendix is dropped htmlId is unused and set to @mempty@
                        addPhantomTocEntry (Success $ toHtml appendixSectionTitle) >> return mempty
                    else
                        addTocEntry Nothing (Success $ Now $ toHtml appendixSectionTitle) Nothing Other
            -- \| Give each Document the corresponding appendix Id
            let zipFunc i nDoc = local (\s -> s {currentAppendixElementID = i}) $ toHtmlM nDoc
            documentHtmls <-
                -- \| Set all necessary formats for appendix document headings
                local
                    ( \s ->
                        s
                            { appendixElementIdFormat = idFormat
                            , appendixElementTocKeyFormat = tocFormat
                            , documentHeadingFormat = Right headFormat
                            }
                    )
                    $ zipWithM zipFunc [1 ..] nodeDocuments
            -- \| Wrap all appendix documents into one <div>
            return $
                if isEmpty
                    then mempty
                    else
                        div_ [cssClass_ Class.AppendixSection, cssClass_ Class.Anchor, id_ htmlId]
                            <$> mconcat documentHtmls

-------------------------------------------------------------------------------

-- | This instance manages which part of the AST is actually translated into HTML;
--   Everything else is just used to build up the needed context (labels, etc.)
instance (ToHtmlM a) => ToHtmlM (Flagged' a) where
    toHtmlM (Flagged renderFlag a) = do
        -- \| Set False to see if child sets it True again
        modify (\s -> s {hasFlagged = False})
        -- \| Set True for children if renderFlag is True, else keep value
        aHtml <-
            local (\s -> s {shouldRender = renderFlag || shouldRender s}) $ toHtmlM a
        -- \| Decide if output is thrown away
        hasFlaggedChild <- gets hasFlagged
        parentRender <- asks shouldRender
        let render = renderFlag || hasFlaggedChild || parentRender
        -- \| Tell parent that we exist
        modify (\s -> s {hasFlagged = True})
        return $ if render then aHtml else mempty

-------------------------------------------------------------------------------

-- = The @Parsed a@ instances makes the rendering robust against parse errors

-- | Returns rendered heading text
instance ToHtmlM (Fallback NavTocHeading) where
    toHtmlM (Fallback (NavTocHeading title)) = returnNow $ toHtml title

-- | Instance used for 'DocumentIntro', 'DocumentExtro', 'DocumentMainBody' and 'DocumentContainerHeader':
--   Emits 'PhantomTocEntry' and parse error box
instance (ToHtmlM a) => ToHtmlM (NavTocHeaded (Parsed a)) where
    toHtmlM (NavTocHeaded (NavTocHeading title) eErrA) =
        let titleHtml = toHtml title
         in case eErrA of
                Left parseError -> do
                    setHasErrors
                    addPhantomTocEntry (Error titleHtml)
                    returnNow $ parseErrorHtml Nothing parseError
                Right a -> do
                    addPhantomTocEntry (Success titleHtml)
                    toHtmlM a

-- | "Fake" section behaviour in error case
instance (ToHtmlM a) => ToHtmlM (SectionFormatted (Parsed a)) where
    toHtmlM (SectionFormatted sectionFormatS eErrA) = case eErrA of
        Left parseErr -> do
            setHasErrors
            -- \| Count error as @Section@, to keep following
            --    @Section@s correctly numbered
            sectionID <- gets currentSectionID
            incSectionID
            -- \| Since we dont have a title we set the tocID as title
            let (_, tocKeyHtml) = sectionFormat sectionFormatS sectionID
            htmlID <- addTocEntry Nothing (Error $ Now tocKeyHtml) Nothing SomeSection
            returnNow $ parseErrorHtml (Just htmlID) parseErr
        Right a -> local (\s -> s {localSectionFormat = sectionFormatS}) $ toHtmlM a

-------------------------------------------------------------------------------

-- | Render current ToC from State
renderLocalToc :: Maybe TocFormat -> HtmlReaderState
renderLocalToc mTocFormat = do
    globalState <- get
    entryFunc <- asks tocEntryWrapperFunc
    buttonFunc <- asks tocButtonWrapperFunc
    return $ renderToc mTocFormat entryFunc buttonFunc globalState

-- | Returns a Later which takes a GlobalState to render a delayed ToC
renderDelayedToc :: Maybe TocFormat -> HtmlReaderState
renderDelayedToc mTocFormat = do
    entryFunc <- asks tocEntryWrapperFunc
    buttonFunc <- asks tocButtonWrapperFunc

    -- \| Return Later to delay ToC generation to the end;
    --    Join Delayed (Delayed (Html ())) together,
    --    since the ToC itself contains Delayed (Html ()) too
    return $ join $ Later $ \globalState -> renderToc mTocFormat entryFunc buttonFunc globalState

-- | Helper function for rendering a ToC from the given  GlobalState
renderToc
    :: Maybe TocFormat
    -> TocEntryWrapper
    -- ^ Wrapper for Toc entry text
    -> TocEntryWrapper
    -- ^ Wrapper for Toc button
    -> GlobalState
    -> Delayed (Html ())
renderToc Nothing _ _ _ = mempty
renderToc (Just (TocFormat (TocHeading title))) entryFunc buttonFunc globalState =
    let colGroup =
            colgroup_
                ( col_ <#> Class.MinSizeColumn
                    <> col_ <#> Class.MaxSizeColumn
                    <> col_ <#> Class.MinSizeColumn
                )
        tableHead =
            thead_ $ tr_ (th_ [colspan_ "3"] (toHtml title))
                :: Html ()

        -- \| Build List of ToC rows
        tocEntries :: [Delayed (Html ())]
        tocEntries =
            -- \| @rights@ filters all phantom entries and unpacks real ones
            let tupleList = rights $ toList $ tableOfContents globalState
             in map (buildWrappedRow entryFunc buttonFunc) tupleList

        -- \| [Delayed (Html ())] -> Delayed [Html ()] -> Delayed (Html ())
        tableBody = tbody_ . mconcat <$> sequence tocEntries
     in (nav_ <#> Class.TableContainer) . (table_ <#> Class.TableOfContents)
            <$> (pure colGroup <> pure tableHead <> tableBody)
  where
    -- \| Build <tr><td>id</td> <td>title</td></tr> and wrap id and title seperatly
    buildWrappedRow
        :: TocEntryWrapper
        -> TocEntryWrapper
        -> TocEntry
        -> Delayed (Html ())
    buildWrappedRow entryWrapper buttonWrapper (mIdHtml, rTitle, htmlId, category) =
        let
            -- \| Draw ToC Error titles as inline errors
            entryWrap = entryWrapper category (Label htmlId)
            buttonWrap = buttonWrapper category (Label htmlId)
            titleHtml = result id (span_ <#> Class.InlineError <$>) rTitle
            titleCell = td_ . entryWrap <$> titleHtml
            idCell = td_ <#> Class.TableCentered $ maybe mempty entryWrap mIdHtml
            linkButton = td_ <#> Class.TableCentered $ buttonWrap $ toHtml ("↗" :: Text)
         in
            -- \| Nothing IdHtmls will be replaced with mempty
            tr_ <$> pure idCell <> titleCell <> pure linkButton

-------------------------------------------------------------------------------

instance (ToHtmlM a) => ToHtmlM [a] where
    toHtmlM [] = returnNow mempty
    toHtmlM (a : as) = do
        aHtml <- toHtmlM a
        asHtml <- toHtmlM as
        return (aHtml <> asHtml)

instance (ToHtmlM a) => ToHtmlM (Maybe a) where
    toHtmlM Nothing = return mempty
    toHtmlM (Just a) = toHtmlM a

-- | ToHtmlM instance that can never be called, because there are
--   no values of type Void
instance ToHtmlM Void where
    toHtmlM = absurd

-------------------------------------------------------------------------------

-- | Groups raw text in <div> and leaves enums as they are
renderDivGrouped
    :: (ToHtmlM fnref, ToCssClass style, ToHtmlM enum, ToHtmlM special)
    => [TextTree lbrek fnref style enum special]
    -> HtmlReaderState
renderDivGrouped = renderGroupedTextTree div_ id

-- | Extracts enums from list and wraps raw text (without enums) into textF_;
--   enums are wrapped into enumF_;
--   E.g. result: <span> raw text </span>
--                <div> <enum></enum> </div>
--                <span> raw reference </span>
renderGroupedTextTree
    :: (ToHtmlM fnref, ToCssClass style, ToHtmlM enum, ToHtmlM special)
    => (Html () -> Html ())
    -> (Html () -> Html ())
    -> [TextTree lbrek fnref style enum special]
    -> HtmlReaderState
renderGroupedTextTree _ _ [] = returnNow mempty
renderGroupedTextTree textF_ enumF_ (enum@(Enum _) : ts) = do
    enumHtml <- toHtmlM enum
    followingHtml <- renderGroupedTextTree textF_ enumF_ ts
    -- \| Wrap enum into enumF_
    return $ (enumF_ <$> enumHtml) <> followingHtml
renderGroupedTextTree textF_ enumF_ tts =
    let (rawText, tts') = getNextRawTextTree tts
     in do
            rawTextHtml <- toHtmlM rawText
            followingHtml <- renderGroupedTextTree textF_ enumF_ tts'
            -- \| Wrap raw text without enums into textF_
            return $ (textF_ <$> rawTextHtml) <> followingHtml

-------------------------------------------------------------------------------

htmlError :: Text -> Html ()
htmlError msg = span_ <#> Class.InlineError $ toHtml ("Error: " <> msg :: Text)

-- | Takes Maybe HtmlId and a parse error and renders a centered parse error box
parseErrorHtml :: Maybe Text -> ParseErrorBundle Text Void -> Html ()
parseErrorHtml mHtmlId errBundle = do
    div_ <#> Class.CenteredBox $
        div_ [cssClass_ Class.ErrorBox, cssClass_ Class.Anchor, mTextId_ mHtmlId] $ do
            h1_ <#> Class.DocumentTitle $ "Parsing failed!"
            pre_ $ code_ <#> Class.LargeFontSize $ toHtml $ errorBundlePretty errBundle
