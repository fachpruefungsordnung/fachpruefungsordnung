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
import Data.Text (Text)
import Data.Void (Void)
import Language.Lsd.AST.Type.Enum (EnumFormat (..), EnumItemFormat (..))
import Language.Ltml.AST.Document
import Language.Ltml.AST.Label
import Language.Ltml.AST.Node
import Language.Ltml.AST.Paragraph
import Language.Ltml.AST.Section
import Language.Ltml.AST.Text
import Language.Ltml.HTML.CSS (mainStylesheet)
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Language.Ltml.HTML.CSS.Util
import Language.Ltml.HTML.Common
import Language.Ltml.HTML.FormatString
import Language.Ltml.HTML.References
import Language.Ltml.HTML.Util
import Lucid
import Prelude hiding (id)

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

renderHtmlCss :: (ToHtmlM a) => a -> (Html (), Css)
renderHtmlCss a =
    let (delayedHtml, finalState) = runState (runReaderT (toHtmlM a) initReaderState) initGlobalState
     in (evalDelayed delayedHtml finalState, mainStylesheet (enumStyles finalState))

-------------------------------------------------------------------------------

class ToHtmlM a where
    toHtmlM :: a -> HtmlReaderState

instance ToHtmlM Document where
    -- \| builds Lucid 2 HTML from a Ltml Document AST
    toHtmlM (Document format header body) = case body of
        DocumentBody [] -> returnNow mempty
        DocumentBody nodes -> toHtmlM nodes

-- TODO: distinguish sections and super-sections based on Left or Right children
--       superSectionID and superSectionIDHtml are already in States.
-- Will super-sections ever be parsed together? Or maybe we intialize the globalState with some superSectionID from the Document type

-- | This combined instances creates the sectionIDHtml before building the reference,
--   which is needed for correct referencing
instance ToHtmlM (Node Section) where
    toHtmlM (Node mLabel (Section sectionFormatS (Heading headingFormatS title) children)) = do
        globalState <- get
        titleHtml <- toHtmlM title
        let (sectionIDHtml, sectionTocKeyHtml) = sectionFormat sectionFormatS (currentSectionID globalState)
            headingHtml =
                (h2_ <#> Class.Heading) . headingFormat headingFormatS sectionIDHtml
                    <$> titleHtml
         in do
                -- \| Add table of contents entry for section
                htmlId <- addTocEntry sectionTocKeyHtml titleHtml mLabel
                -- \| Build heading Html with sectionID
                childrenHtml <- local (\s -> s {currentSectionIDHtml = sectionIDHtml}) $ do
                    addMaybeLabelToState mLabel SectionRef
                    case children of
                        Right cs -> toHtmlM cs
                        -- \| In the Left case the children are paragraphs, so we set the needed flag for them
                        --   to decide if the should have a visible id
                        Left cs -> local (\s -> s {isSingleParagraph = length cs == 1}) $ toHtmlM cs
                -- \| increment sectionID for next section
                modify (\s -> s {currentSectionID = currentSectionID s + 1})
                -- \| reset paragraphID for next section
                modify (\s -> s {currentParagraphID = 1})

                return $
                    section_ [cssClass_ Class.Section, id_ htmlId] <$> (headingHtml <> childrenHtml)

instance ToHtmlM (Node Paragraph) where
    toHtmlM (Node mLabel (Paragraph format textTrees)) = do
        globalState <- get
        let (paragraphIDHtml, paragraphKeyHtml) = paragraphFormat format (currentParagraphID globalState)
         in do
                childText <- local (\s -> s {currentParagraphIDHtml = paragraphIDHtml}) $ do
                    addMaybeLabelToState mLabel ParagraphRef
                    -- \| Group raw text (without enums) into <div> for flex layout spacing
                    renderGroupedTextTree textTrees
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

instance
    (ToHtmlStyle style, ToHtmlM enum, ToHtmlM special)
    => ToHtmlM (TextTree style enum special)
    where
    toHtmlM textTree = case textTree of
        Word text -> returnNow $ toHtml text
        Space -> returnNow $ toHtml (" " :: Text)
        Special special -> toHtmlM special
        Reference label -> return $ Later $ \globalState ->
            case lookup (unLabel label) $ labels globalState of
                -- \| Label was not found in GlobalState and a red error is emitted
                Nothing ->
                    span_ <#> Class.InlineError $
                        toHtml (("Error: Label \"" <> unLabel label <> "\" not found!") :: Text)
                Just labelHtml -> labelWrapperFunc globalState label labelHtml
        Styled style textTrees -> do
            textTreeHtml <- toHtmlM textTrees
            return $ toHtmlStyle style <$> textTreeHtml
        Enum enum -> toHtmlM enum
        Footnote _ ->
            returnNow $
                span_ <#> Class.InlineError $
                    toHtml ("Error: FootNotes not supported yet" :: Text)

-- | Increment sentence counter and add Label to GlobalState, if there is one
instance ToHtmlM SentenceStart where
    toHtmlM (SentenceStart mLabel) = do
        modify (\s -> s {currentSentenceID = currentSentenceID s + 1})
        addMaybeLabelToState mLabel SentenceRef
        case mLabel of
            Nothing -> returnNow mempty
            Just label -> returnNow $ span_ [id_ (unLabel label)] mempty

class ToHtmlStyle style where
    toHtmlStyle :: (Monad m) => style -> (HtmlT m a -> HtmlT m a)

instance ToHtmlStyle FontStyle where
    toHtmlStyle Bold = span_ <#> Class.Bold
    toHtmlStyle Italics = span_ <#> Class.Italic
    toHtmlStyle Underlined = span_ <#> Class.Underlined

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
        addMaybeLabelToState mLabel EnumItemRef
        -- \| Save current enum item id, if nested enumerations follow and reset it
        enumItemID <- gets currentEnumItemID
        -- \| Render grouped raw text (without enums) to get correct flex spacing
        enumItemHtml <- renderGroupedTextTree textTrees
        -- \| Increment enumItemID for next enumItem
        modify (\s -> s {currentEnumItemID = enumItemID + 1})
        return $
            div_ [cssClass_ Class.TextContainer, mId_ mLabel] <$> enumItemHtml

instance (ToHtmlM a) => ToHtmlM [a] where
    toHtmlM [] = returnNow mempty
    toHtmlM (a : as) = do
        aHtml <- toHtmlM a
        asHtml <- toHtmlM as
        return (aHtml <> asHtml)

-------------------------------------------------------------------------------

-- | ToHtmlM instance that can never be called, because there are
--   no values of type Void
instance ToHtmlM Void where
    toHtmlM = error "toHtmlM for Void was called!"

-- | ToHtmlStyle instance that can never be called, because there are
--   no values of type Void
instance ToHtmlStyle Void where
    toHtmlStyle = error "toHtmlStyle for Void was called!"

-------------------------------------------------------------------------------

-- | Extracts enums from list and packs raw text (without enums) into <div>;
--   E.g. result: <div> raw text </div> <enum></enum> <div> reference </div>
renderGroupedTextTree
    :: (ToHtmlStyle style, ToHtmlM enum, ToHtmlM special)
    => [TextTree style enum special]
    -> HtmlReaderState
renderGroupedTextTree [] = returnNow mempty
renderGroupedTextTree (enum@(Enum _) : ts) = do
    enumHtml <- toHtmlM enum
    followingHtml <- renderGroupedTextTree ts
    return $ enumHtml <> followingHtml
renderGroupedTextTree tts =
    let (rawText, tts') = getNextRawTextTree tts
     in do
            rawTextHtml <- toHtmlM rawText
            followingHtml <- renderGroupedTextTree tts'
            -- \| Pack raw text without enums into <div>
            return $ (div_ <$> rawTextHtml) <> followingHtml
