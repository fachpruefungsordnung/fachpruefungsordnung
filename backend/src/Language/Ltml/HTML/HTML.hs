{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Language.Ltml.HTML.HTML (ToHtmlM (..), renderHtml, docToHtml, sectionToHtml, addHtmlHeader, aToHtml) where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text, pack)
import Data.Void (Void)
import Language.Ltml.AST.Document
import Language.Ltml.AST.Label
import Language.Ltml.AST.Node
import Language.Ltml.AST.Paragraph
import Language.Ltml.AST.Section
import Language.Ltml.AST.Text
import Language.Ltml.HTML.CSS.Classes (enumLevel)
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
docToHtml = aToHtml "Test Dokument" "out.css"

sectionToHtml :: Node Section -> Html ()
sectionToHtml = aToHtml "Test Dokument" "out.css"

-- | Internal function that renders to Html and creates final HTML wrapper
--   with title and a path to a css file
aToHtml :: (ToHtmlM a) => String -> FilePath -> a -> Html ()
aToHtml title relativeCssPath a =
    let (delayedHtml, finalState) = runState (runReaderT (toHtmlM a) initReaderState) initGlobalState
        body = evalDelayed delayedHtml finalState
     in addHtmlHeader title relativeCssPath body

-- | Adds html, head and body tags onto given html and
--   sets title and css path
addHtmlHeader :: String -> FilePath -> Html () -> Html ()
addHtmlHeader title cssPath html = doctypehtml_ $ do
    head_ $ do
        title_ (toHtml title)
        link_ [rel_ "stylesheet", href_ (pack cssPath)]
    body_ html

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
    toHtmlM (Node mLabel (Section format heading children)) = do
        globalState <- get
        let sectionIDHtml = sectionFormat format (currentSectionID globalState)
         in do
                sectionHtml <- local (\s -> s {currentSectionIDHtml = sectionIDHtml}) $ do
                    addMaybeLabelToState mLabel SectionRef
                    headingHtml <- toHtmlM heading
                    childrenHtml <- case children of
                        Right cs -> toHtmlM cs
                        -- \| In the Left case the children are paragraphs, so we set the needed flag for them
                        --   to decide if the should have a visible id
                        Left cs -> local (\s -> s {isSingleParagraph = length cs == 1}) $ toHtmlM cs
                    return $ headingHtml <> childrenHtml
                -- \| increment sectionID for next section
                modify (\s -> s {currentSectionID = currentSectionID s + 1})
                -- \| reset paragraphID for next section
                modify (\s -> s {currentParagraphID = 1})

                return $ div_ [cssClass_ Class.Section, mId_ mLabel] <$> sectionHtml

-- | Instance for Heading of a Section
instance ToHtmlM Heading where
    toHtmlM (Heading format textTree) = do
        headingTextHtml <- toHtmlM textTree
        readerState <- ask
        return
            ( (h4_ <#> Class.Heading)
                . headingFormat format (currentSectionIDHtml readerState)
                <$> headingTextHtml
            )

instance ToHtmlM (Node Paragraph) where
    toHtmlM (Node mLabel (Paragraph format textTrees)) = do
        globalState <- get
        let (paragraphIDHtml, mParagraphIDRawHtml) = paragraphFormat format (currentParagraphID globalState)
         in do
                childText <- local (\s -> s {mCurrentParagraphIDHtml = mParagraphIDRawHtml}) $ do
                    addMaybeLabelToState mLabel ParagraphRef
                    toHtmlM textTrees
                modify (\s -> s {currentParagraphID = currentParagraphID s + 1})
                -- \| Reset sentence id for next paragraph
                modify (\s -> s {currentSentenceID = 0})
                readerState <- ask
                return $
                    div_ [cssClass_ Class.Paragraph, mId_ mLabel]
                        -- \| If this is the only paragraph inside this section we drop the visible paragraphID
                        <$> let idHtml = if isSingleParagraph readerState then mempty else paragraphIDHtml
                             in return (div_ <#> Class.ParagraphID $ idHtml) <> div_ <#> Class.ParagraphText
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
                    b_ <#> Class.FontRed $
                        toHtml (("Error: Label \"" <> unLabel label <> "\" not found!") :: Text)
                Just labelHtml -> labelWrapperFunc globalState label labelHtml
        Styled style textTrees -> do
            textTreeHtml <- toHtmlM textTrees
            return $ toHtmlStyle style <$> textTreeHtml
        Enum enum -> toHtmlM enum
        Footnote _ ->
            returnNow $
                b_ <#> Class.FontRed $
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
    toHtmlStyle Bold = b_
    toHtmlStyle Italics = i_
    toHtmlStyle Underlined = span_ <#> Class.Underlined

instance ToHtmlM Enumeration where
    toHtmlM (Enumeration enumItems) = do
        readerState <- ask
        -- \| Reset enumItemID for this Enumeration
        modify (\s -> s {currentEnumItemID = 1})
        nested <-
            mapM
                (local (\s -> s {enumNestingLevel = enumNestingLevel s + 1}) . toHtmlM)
                enumItems
        return $ do
            nestedHtml <- sequence nested
            let enumItemsHtml = foldr ((>>) . li_) (mempty :: Html ()) nestedHtml
             in return $ ol_ <#> enumLevel (enumNestingLevel readerState) $ enumItemsHtml

instance ToHtmlM (Node EnumItem) where
    toHtmlM (Node mLabel (EnumItem textTrees)) = do
        addMaybeLabelToState mLabel EnumItemRef
        -- \| Save current enum item id, if nested enumerations follow and reset it
        enumItemID <- gets currentEnumItemID
        enumItemHtml <- toHtmlM textTrees
        -- \| Increment enumItemID for next enumItem
        modify (\s -> s {currentEnumItemID = enumItemID + 1})
        return $ span_ [cssClass_ Class.EnumItem, mId_ mLabel] <$> enumItemHtml

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
