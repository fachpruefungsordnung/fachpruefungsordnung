{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Language.Ltml.HTML.HTML (renderHtml, docToHtml, sectionToHtml) where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Void (Void)
import Language.Ltml.AST.Document
import Language.Ltml.AST.Label
import Language.Ltml.AST.Node
import Language.Ltml.AST.Paragraph
import Language.Ltml.AST.Section
import Language.Ltml.AST.Text
import Language.Ltml.HTML.CSS.Classes (enumLevel)
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Language.Ltml.HTML.Common
import Language.Ltml.HTML.FormatString
import Language.Ltml.HTML.References
import Lucid
import Prelude hiding (id)

renderHtml :: Document -> ByteString
renderHtml document = renderBS $ docToHtml document

docToHtml :: Document -> Html ()
docToHtml = aToHtml

sectionToHtml :: Node Section -> Html ()
sectionToHtml = aToHtml

-- | Internal function that creates final HTML wrapper and header
aToHtml :: (ToHtmlM a) => a -> Html ()
aToHtml a = doctypehtml_ $ do
    head_ $ do
        title_ "Test Dokument"
        link_ [rel_ "stylesheet", href_ "out.css"]
    body_ $ do
        evalState (runReaderT (toHtmlM a) initReaderState) initGlobalState

-------------------------------------------------------------------------------

class ToHtmlM a where
    toHtmlM :: a -> ReaderT ReaderState (State GlobalState) (Html ())

instance ToHtmlM Document where
    -- \| builds Lucid 2 HTML from a Ltml Document AST
    toHtmlM (Document format header body) = case body of
        DocumentBody [] -> return $ return ()
        DocumentBody nodes -> toHtmlM nodes

-- | If a Label is present, it will be added to the GlobalState
--   but it wont be visible in HTML
-- instance (ToHtmlM a) => ToHtmlM (Node a) where
--     toHtmlM (Node maybeLabel a) = case maybeLabel of
--         Nothing -> toHtmlM a
--         Just label -> do
--             -- \| TODO: somehow track if we have a label for a section or a paragraph
--             addLabelToState label SectionRef
--             toHtmlM a

-- | This combined instances creates the sectionIDHtml before building the reference,
--   which is needed for correct referencing
instance ToHtmlM (Node Section) where
    toHtmlM (Node mLabel (Section format heading children)) = do
        globalState <- get
        let sectionIDHtml = sectionFormat format (currentSectionID globalState)
         in do
                sectionHtml <- local (\s -> s {currentSectionIDHtml = sectionIDHtml}) $ do
                    case mLabel of
                        Nothing -> return ()
                        Just label -> addLabelToState label SectionRef
                    headingHtml <- toHtmlM heading
                    childrenHtml <- case children of
                        Right cs -> toHtmlM cs
                        Left cs -> toHtmlM cs
                    return $ headingHtml <> childrenHtml
                -- \| increment sectionID for next section
                modify (\s -> s {currentSectionID = currentSectionID s + 1})
                -- \| reset paragraphID for next section
                modify (\s -> s {currentParagraphID = 1})

                return sectionHtml

-- instance ToHtmlM Section where
--     toHtmlM (Section format heading children) = do
--         globalState <- get
--         let sectionIDHtml = sectionFormat format (currentSectionID globalState)
--          in do
--                 sectionHtml <- local (\s -> s {currentSectionIDHtml = sectionIDHtml}) $ do
--                     headingHtml <- toHtmlM heading
--                     childrenHtml <- case children of
--                         Right cs -> toHtmlM cs
--                         Left cs -> toHtmlM cs
--                     return $ headingHtml <> childrenHtml
--                 -- \| increment sectionID for next section
--                 modify (\s -> s {currentSectionID = currentSectionID s + 1})
--                 -- \| reset paragraphID for next section
--                 modify (\s -> s {currentParagraphID = 1})

--                 return sectionHtml

-- | Instance for Heading of a Section
instance ToHtmlM Heading where
    toHtmlM (Heading format textTree) = do
        headingTextHtml <- toHtmlM textTree
        readerState <- ask
        return $
            h4_ [class_ (Class.className Class.Centered)] $
                headingFormat format (currentSectionIDHtml readerState) headingTextHtml

instance ToHtmlM (Node Paragraph) where
    toHtmlM (Node mLabel (Paragraph format textTrees)) = do
        globalState <- get
        let (paragraphIDHtml, mParagraphIDRawHtml) = paragraphFormat format (currentParagraphID globalState)
         in do
                childText <- local (\s -> s {mCurrentParagraphIDHtml = mParagraphIDRawHtml}) $ do
                    case mLabel of
                        Nothing -> return ()
                        Just label -> addLabelToState label ParagraphRef
                    toHtmlM textTrees
                modify (\s -> s {currentParagraphID = currentParagraphID s + 1})
                -- \| Reset sentence id for next paragraph
                modify (\s -> s {currentSentenceID = 0})
                return $ paragraphIDHtml <> childText

-- instance ToHtmlM Paragraph where
--     toHtmlM :: Paragraph -> ReaderT ReaderState (State GlobalState) (Html ())
--     toHtmlM (Paragraph format textTrees) = do
--         globalState <- get
--         let (paragraphIDHtml, mParagraphIDRawHtml) = paragraphFormat format (currentParagraphID globalState)
--          in do
--                 childText <-
--                     local (\s -> s {mCurrentParagraphIDHtml = mParagraphIDRawHtml}) $
--                         toHtmlM textTrees
--                 modify (\s -> s {currentParagraphID = currentParagraphID s + 1})
--                 -- \| Reset sentence id for next paragraph
--                 modify (\s -> s {currentSentenceID = 0})
--                 return $ paragraphIDHtml <> childText

instance
    (ToHtmlStyle style, ToHtmlM enum, ToHtmlM special)
    => ToHtmlM (TextTree style enum special)
    where
    toHtmlM textTree = case textTree of
        Word text -> return $ toHtml text
        Space -> return $ toHtml (" " :: Text)
        Special special -> toHtmlM special
        Reference label -> do
            globalState <- get
            case lookup (unLabel label) $ labels globalState of
                -- \| Label was not found in GlobalState and a red error is emitted
                Nothing ->
                    return $
                        b_ [class_ (Class.className Class.FontRed)] $
                            toHtml (("Error: Label \"" <> unLabel label <> "\" not found!") :: Text)
                Just labelHtml -> return labelHtml
        Styled style textTrees -> toHtmlStyle style <$> toHtmlM textTrees
        Enum enum -> toHtmlM enum
        Footnote _ -> return $ toHtml ("Error: FootNotes not supported yet" :: Text)

-- | Increment sentence counter and add Label to GlobalState, if there is one
instance ToHtmlM SentenceStart where
    toHtmlM (SentenceStart mLabel) = do
        modify (\s -> s {currentSentenceID = currentSentenceID s + 1})
        maybe (return ()) (\l -> addLabelToState l SentenceRef) mLabel
        return mempty

class ToHtmlStyle style where
    toHtmlStyle :: (Monad m) => style -> (HtmlT m a -> HtmlT m a)

instance ToHtmlStyle FontStyle where
    toHtmlStyle Bold = b_
    toHtmlStyle Italics = i_
    toHtmlStyle Underlined = span_ [class_ (Class.className Class.Underlined)]

instance ToHtmlM Enumeration where
    toHtmlM (Enumeration enumItems) = do
        readerState <- ask
        nested <-
            mapM
                (local (\s -> s {enumNestingLevel = enumNestingLevel s + 1}) . toHtmlM)
                enumItems
        return $
            ol_ [class_ $ enumLevel (enumNestingLevel readerState)] $
                foldr ((>>) . li_) mempty nested

instance ToHtmlM EnumItem where
    toHtmlM (EnumItem textTrees) = toHtmlM textTrees

instance (ToHtmlM a) => ToHtmlM [a] where
    toHtmlM [] = return mempty
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