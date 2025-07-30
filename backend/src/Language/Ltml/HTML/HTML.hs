{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

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
import Language.Ltml.HTML.FormatString
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
aToHtml a = html_ $ do
    head_ $ do
        title_ "Test Dokument"
        link_ [rel_ "stylesheet", href_ "out.css"]
    body_ $ do
        evalState (runReaderT (toHtmlM a) initReaderState) initGlobalState

data GlobalState = GlobalState
    { currentSectionID :: Int
    -- ^ tracks the current section numbering
    , currentParagraphID :: Int
    -- ^ tracks the current paragraph numbering
    , labels :: [(Text, Html ())]
    }

data ReaderState = ReaderState
    { enumNestingLevel :: Int
    -- ^ tracks the current enumeration nesting level
    , currentSectionIDHtml :: Html ()
    -- ^ holds the actual Html numbering that should be displayed for the current section
    }

initGlobalState :: GlobalState
initGlobalState =
    GlobalState
        { currentSectionID = 1
        , currentParagraphID = 1
        }

initReaderState :: ReaderState
initReaderState =
    ReaderState
        { enumNestingLevel = 0
        , currentSectionIDHtml = mempty
        }

class ToHtmlM a where
    toHtmlM :: a -> ReaderT ReaderState (State GlobalState) (Html ())

instance ToHtmlM Document where
    -- \| builds Lucid 2 HTML from a Ltml Document AST
    toHtmlM (Document format header body) = case body of
        DocumentBody [] -> return $ return ()
        DocumentBody nodes -> toHtmlM nodes

instance (ToHtmlM a) => ToHtmlM (Node a) where
    toHtmlM (Node maybeLabel a) = case maybeLabel of
        Nothing -> toHtmlM a
        Just label -> do
            labelHtml <- b_ <$> toHtmlM label
            aHtml <- toHtmlM a
            return (labelHtml <> aHtml)

instance ToHtmlM Section where
    toHtmlM (Section format heading children) = do
        globalState <- get
        let sectionIDHtml = sectionFormat format (currentSectionID globalState)
         in do
                headingHtml <-
                    local (\s -> s {currentSectionIDHtml = sectionIDHtml}) $ toHtmlM heading
                childrenHtml <- case children of
                    Right cs -> toHtmlM cs
                    Left cs -> toHtmlM cs
                -- \| increment sectionID for next section
                modify (\s -> s {currentSectionID = currentSectionID s + 1})
                -- \| reset paragraphID for next section
                modify (\s -> s {currentParagraphID = 1})

                return $ headingHtml <> childrenHtml

-- | Instance for Heading of a Section
instance ToHtmlM Heading where
    toHtmlM (Heading format textTree) = do
        headingTextHtml <- toHtmlM textTree
        readerState <- ask
        return $
            h4_ [class_ (Class.className Class.Centered)] $
                headingFormat format (currentSectionIDHtml readerState) headingTextHtml

instance ToHtmlM Paragraph where
    toHtmlM :: Paragraph -> ReaderT ReaderState (State GlobalState) (Html ())
    toHtmlM (Paragraph format textTrees) = do
        childText <- toHtmlM textTrees
        globalState <- get
        let curParaID = currentParagraphID globalState
         in do
                modify (\s -> s {currentParagraphID = currentParagraphID s + 1})
                return $ paragraphFormat format curParaID <> childText

instance (ToHtmlStyle style, ToHtmlM enum) => ToHtmlM (TextTree style enum special) where
    toHtmlM textTree = case textTree of
        Word text -> return $ toHtml text
        Space -> return $ toHtml (" " :: Text)
        Special _ -> return $ toHtml ("Error: Special not supported yet" :: Text)
        Reference label -> toHtmlM label
        Styled style textTrees -> toHtmlStyle style <$> toHtmlM textTrees
        Enum enum -> toHtmlM enum
        Footnote _ -> return $ toHtml ("Error: FootNotes not supported yet" :: Text)

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

instance ToHtmlM Label where
    toHtmlM label = return $ toHtml $ unLabel label
    -- TODO: define Trie Map in GlobalState to track label references
    -- toHtmlM label = do
    --     modify (\s -> s {labels = (unLabel label, toHtml "Section 1" :: Html ()) : labels s})
    --     return mempty


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
