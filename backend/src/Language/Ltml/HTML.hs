{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Language.Ltml.HTML (ToHtmlM (..), renderHtml, docToHtml, sectionToHtml, aToHtml, renderHtmlCss) where

import Clay (Css)
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text, unpack)
import Data.Void (Void, absurd)
import Language.Lsd.AST.Type.Enum (EnumFormat (..), EnumItemFormat (..))
import Language.Lsd.AST.Type.SimpleParagraph (SimpleParagraphFormat (..))
import Language.Ltml.AST.AppendixSection (AppendixSection (..))
import Language.Ltml.AST.Document
import Language.Ltml.AST.DocumentContainer (DocumentContainer (..))
import Language.Ltml.AST.Footnote (Footnote (..))
import Language.Ltml.AST.Label
import Language.Ltml.AST.Node
import Language.Ltml.AST.Paragraph
import Language.Ltml.AST.Section
import Language.Ltml.AST.SimpleBlock (SimpleBlock (..))
import Language.Ltml.AST.SimpleParagraph (SimpleParagraph (..))
import Language.Ltml.AST.SimpleSection (SimpleSection (..))
import Language.Ltml.AST.Table (Table)
import Language.Ltml.AST.Text
import Language.Ltml.HTML.CSS (mainStylesheet)
import Language.Ltml.HTML.CSS.Classes (ToCssClass (toCssClass))
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Language.Ltml.HTML.CSS.Util
import Language.Ltml.HTML.Common
import Language.Ltml.HTML.FormatString
import Language.Ltml.HTML.References
import Language.Ltml.HTML.Util
import Lucid
import Prelude

renderHtml :: Document -> ByteString
renderHtml document = renderBS $ docToHtml document

docToHtml :: Document -> Html ()
docToHtml doc = addHtmlHeader "Test Dokument" "out.css" (aToHtml doc)

sectionToHtml :: Node Section -> Html ()
sectionToHtml sec = addHtmlHeader "Test Dokument" "out.css" (aToHtml sec)

-- | Internal function that renders to Html
aToHtml :: (ToHtmlM a) => a -> Html ()
aToHtml a =
    let (delayedHtml, finalState) = runState (runReaderT (toHtmlM a) initReaderState) initGlobalState
     in evalDelayed delayedHtml finalState

renderHtmlCss :: Node Section -> Map.Map Label Footnote -> (Html (), Css)
renderHtmlCss section fnMap =
    -- \| Render with given footnote context
    let readerState = initReaderState {footnoteMap = fnMap}
        (delayedHtml, finalState) = runState (runReaderT (toHtmlM section) readerState) initGlobalState
        usedFootnotes =
            map (\(label, (_, idHtml, _)) -> (label, idHtml)) $ usedFootnoteMap finalState
        -- \| Add footnote labes for "normal" (non-footnote) references
        finalState' = finalState {labels = usedFootnotes ++ labels finalState}
     in (evalDelayed delayedHtml finalState', mainStylesheet (enumStyles finalState))

-------------------------------------------------------------------------------

class ToHtmlM a where
    toHtmlM :: a -> HtmlReaderState

instance ToHtmlM DocumentContainer where
    toHtmlM (DocumentContainer format header doc appendices) = do
        mainDocHtml <- toHtmlM doc
        appendicesHtml <- toHtmlM appendices
        return $ mainDocHtml <> appendicesHtml

instance ToHtmlM (Node Document) where
    -- \| TODO: Render with context: Am i inside of an appendix?
    toHtmlM (Node mLabel doc) = undefined

instance ToHtmlM Document where
    -- \| builds Lucid 2 HTML from a Ltml Document AST
    toHtmlM
        ( Document
                format
                (DocumentTitle title)
                (DocumentBody introSSections sectionBody outroSSections)
                footNotes
            ) =
            let titleHtml = h1_ <#> Class.DocumentTitle $ toHtml title
             in local (\s -> s {footnoteMap = footNotes}) $ do
                    -- \| Reset used footnotes (footnotes are document-scoped)
                    modify (\s -> s {usedFootnoteMap = usedFootnoteMap initGlobalState})
                    introHtml <- toHtmlM introSSections
                    mainHtml <- toHtmlM sectionBody
                    outroHtml <- toHtmlM outroSSections
                    return $ Now titleHtml <> introHtml <> mainHtml <> outroHtml

-- | This combined instances creates the sectionIDHtml before building the reference,
--   which is needed for correct referencing
instance ToHtmlM (Node Section) where
    toHtmlM (Node mLabel (Section sectionFormatS (Heading headingFormatS title) sectionBody)) = do
        globalState <- get
        titleHtml <- toHtmlM title
        let (sectionIDGetter, incrementSectionID, sectionCssClass) =
                -- \| Check if we are inside a section or a super-section
                -- TODO: Is [SimpleBlocks] counted as super-section? (i think yes)
                if isSuper sectionBody
                    then (currentSuperSectionID, incSuperSectionID, Class.SuperSection)
                    else (currentSectionID, incSectionID, Class.Section)
            (sectionIDHtml, sectionTocKeyHtml) = sectionFormat sectionFormatS (sectionIDGetter globalState)
            headingHtml =
                (h2_ <#> Class.Heading) . headingFormat headingFormatS sectionIDHtml
                    <$> titleHtml
         in do
                addMaybeLabelToState mLabel sectionIDHtml
                -- \| Add table of contents entry for section
                htmlId <- addTocEntry sectionTocKeyHtml titleHtml mLabel
                -- \| Build heading Html with sectionID
                childrenHtml <- toHtmlM sectionBody
                -- \| Also render footnotes in super-sections, since their heading
                --    could contain footnoteRefs
                childrensGlobalState <- get
                footnotesHtml <- toHtmlM (locallyUsedFootnotes childrensGlobalState)
                -- \| Reset locally used set to inital value
                modify (\s -> s {locallyUsedFootnotes = locallyUsedFootnotes initGlobalState})
                -- \| increment (super)SectionID for next section
                incrementSectionID

                return $
                    section_ [cssClass_ sectionCssClass, id_ htmlId]
                        <$> (headingHtml <> childrenHtml <> footnotesHtml)

instance ToHtmlM SectionBody where
    toHtmlM sectionBody = case sectionBody of
        -- \| Super Section
        InnerSectionBody nodeSections -> toHtmlM nodeSections
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
                    div_ [cssClass_ Class.Paragraph, mId_ mLabel]
                        -- \| If this is the only paragraph inside this section we drop the visible paragraphID
                        <$> let idHtml = if isSingleParagraph readerState then mempty else paragraphKeyHtml
                             in return (div_ <#> Class.ParagraphID $ idHtml) <> div_ <#> Class.TextContainer
                                    <$> childText

-------------------------------------------------------------------------------

-- | Wrapper for block of SimpleParagraphs and table
instance ToHtmlM SimpleBlock where
    toHtmlM simpleBlock = case simpleBlock of
        SimpleParagraphBlock simpleParagraph -> toHtmlM simpleParagraph
        TableBlock table -> toHtmlM table

-- | Section without Heading and Identifier
instance ToHtmlM SimpleSection where
    toHtmlM (SimpleSection sSectionFormat sParagraphs) = do
        paragraphsHtml <- toHtmlM sParagraphs
        return $ section_ <#> Class.Section <$> paragraphsHtml

-- | Paragraph without identifier
instance ToHtmlM SimpleParagraph where
    toHtmlM (SimpleParagraph (SimpleParagraphFormat textAlign fontSize) textTrees) = do
        childText <- toHtmlM textTrees
        return $
            div_
                [ cssClass_ Class.TextContainer
                , cssClass_ $ toCssClass textAlign
                , cssClass_ $ toCssClass fontSize
                ]
                <$> childText

-------------------------------------------------------------------------------

instance ToHtmlM Table where
    toHtmlM table =
        returnNow $
            span_ <#> Class.InlineError $
                toHtml ("Error: Tables are not supported yet!" :: Text)

-------------------------------------------------------------------------------

instance
    (ToHtmlM fnref, ToCssClass style, ToHtmlM enum, ToHtmlM special)
    => ToHtmlM (TextTree fnref style enum special)
    where
    toHtmlM textTree = case textTree of
        Word text -> returnNow $ toHtml text
        Space -> returnNow $ toHtml (" " :: Text)
        Special special -> toHtmlM special
        Reference label -> return $ Later $ \globalState ->
            case lookup label $ labels globalState of
                -- \| Label was not found in GlobalState and a red error is emitted
                Nothing ->
                    span_ <#> Class.InlineError $
                        toHtml (("Error: Label \"" <> unLabel label <> "\" not found!") :: Text)
                Just labelHtml -> labelWrapperFunc globalState label labelHtml
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
            Just label -> returnNow $ span_ [id_ (unLabel label)] mempty

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
            div_ [cssClass_ Class.TextContainer, mId_ mLabel] <$> enumItemHtml

instance ToHtmlM FootnoteReference where
    toHtmlM (FootnoteReference label) = do
        usedFootnotes <- gets usedFootnoteMap
        -- \| Check if footnote was already referenced (document-scoped)
        let mFootnoteIdText = lookup label usedFootnotes
        case mFootnoteIdText of
            -- \| TODO: add anchor links to actual footnote text (in export version)
            Just (footnoteID, footnoteIdHtml, _) -> createFootnoteRef footnoteIdHtml footnoteID label
            Nothing -> do
                -- \| Look for label in footnoteMap with unused footnotes
                unusedFootnoteMap <- asks footnoteMap
                case Map.lookup label unusedFootnoteMap of
                    -- \| Footnote Label does not exist
                    Nothing ->
                        returnNow $ htmlError $ "Footnote Label \"" <> unLabel label <> "\" not found!"
                    Just footnote -> do
                        footnoteID <- gets currentFootnoteID
                        let footnoteIdHtml = toHtml $ show footnoteID
                        footnoteTextHtml <- toHtmlM footnote
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
            returnNow $ sup_ footHtml

instance ToHtmlM Footnote where
    toHtmlM (Footnote format textTrees) = do
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
                        ( (div_ <#> Class.Footnote <$> ((sup_ <#> Class.FootnoteID) idHtml <>)) . span_
                            <$> delayedTextHtml
                        )

-------------------------------------------------------------------------------

instance ToHtmlM AppendixSection where
    toHtmlM (AppendixSection format nodeDocuments) = do
        appendixSectionID <- gets currentAppendixSectionID
        let (titleHtml, headingTextHtml, tocHtml) = appendixFormat format appendixSectionID
        -- \| Mangled ToC Entry as Html Id
        htmlId <- addTocEntry tocHtml (Now titleHtml) Nothing
        documentsHtml <- toHtmlM nodeDocuments
        modify (\s -> s {currentAppendixSectionID = currentAppendixSectionID s + 1})
        let headingHtml = h1_ <#> Class.DocumentTitle $ headingTextHtml
        return ((div_ [id_ htmlId] <> (headingHtml <>)) . div_ <$> documentsHtml)

-------------------------------------------------------------------------------

instance (ToHtmlM a) => ToHtmlM [a] where
    toHtmlM [] = returnNow mempty
    toHtmlM (a : as) = do
        aHtml <- toHtmlM a
        asHtml <- toHtmlM as
        return (aHtml <> asHtml)

-- | ToHtmlM instance that can never be called, because there are
--   no values of type Void
instance ToHtmlM Void where
    toHtmlM = absurd

-------------------------------------------------------------------------------

-- | Groups raw text in <div> and leaves enums as they are
renderDivGrouped
    :: (ToHtmlM fnref, ToCssClass style, ToHtmlM enum, ToHtmlM special)
    => [TextTree fnref style enum special]
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
    -> [TextTree fnref style enum special]
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
