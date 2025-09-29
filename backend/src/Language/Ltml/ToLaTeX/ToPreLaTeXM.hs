{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides a way to convert the AST into the intermediate structure PreLaTeX.
-- To this end a polymorphic typeclass ToPreLaTeXM that provides the function toPreLaTeXM is introduced
-- and instances for all datatypes of the AST are defined.
module Language.Ltml.ToLaTeX.ToPreLaTeXM (ToPreLaTeXM (..))
where

import Control.Lens (use, (%=), (.=))
import Control.Monad.State (State)
import qualified Data.DList as DList
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void, absurd)
import Language.Lsd.AST.Format
    ( EnumItemKeyFormat (EnumItemKeyFormat)
    , HeadingFormat (HeadingFormat)
    , ParagraphKeyFormat (ParagraphKeyFormat)
    , TocKeyFormat (TocKeyFormat)
    )
import Language.Lsd.AST.Type.AppendixSection
    ( AppendixElementFormat (AppendixElementFormat)
    , AppendixSectionFormat (AppendixSectionFormat)
    , AppendixSectionTitle (AppendixSectionTitle)
    )
import Language.Lsd.AST.Type.Document
    ( DocumentFormat (..)
    , TocFormat (TocFormat)
    , TocHeading (TocHeading)
    )
import Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerFormat (DocumentContainerFormat)
    , MainDocumentFormat (MainDocumentFormat)
    )
import Language.Lsd.AST.Type.Enum
    ( EnumFormat (..)
    , EnumItemFormat (EnumItemFormat)
    )
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (ParagraphFormat))
import Language.Lsd.AST.Type.Section
    ( SectionFormat (SectionFormat)
    , SectionFormatted (SectionFormatted)
    )
import Language.Lsd.AST.Type.SimpleParagraph
    ( SimpleParagraphFormat (SimpleParagraphFormat)
    )
import Language.Lsd.AST.Type.SimpleSection
    ( SimpleSectionFormat (SimpleSectionFormat)
    )
import Language.Ltml.AST.AppendixSection (AppendixSection (AppendixSection))
import Language.Ltml.AST.Document
    ( Document (..)
    , DocumentBody (..)
    , DocumentHeading (DocumentHeading)
    )
import Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (DocumentContainer)
    , DocumentContainerHeader (DocumentContainerHeader)
    )
import Language.Ltml.AST.Footnote (Footnote (Footnote))
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.AST.Paragraph (Paragraph (..))
import Language.Ltml.AST.Section
    ( Heading (..)
    , Section (..)
    , SectionBody (InnerSectionBody, LeafSectionBody, SimpleLeafSectionBody)
    )
import Language.Ltml.AST.SimpleBlock
    ( SimpleBlock (SimpleParagraphBlock, TableBlock)
    )
import Language.Ltml.AST.SimpleParagraph (SimpleParagraph (SimpleParagraph))
import Language.Ltml.AST.SimpleSection (SimpleSection (SimpleSection))
import Language.Ltml.AST.Table (Table (Table))
import Language.Ltml.AST.Text
    ( EnumItem (..)
    , Enumeration (..)
    , FootnoteReference (..)
    , HardLineBreak (..)
    , SentenceStart (..)
    , TextTree (..)
    )
import Language.Ltml.Common
    ( Flagged (Flagged)
    , Flagged'
    , NavTocHeaded (NavTocHeaded)
    , Parsed
    )
import Language.Ltml.ToLaTeX.Format
    ( Stylable (..)
    , formatHeading
    , getEnumStyle
    , getIdentifier
    )
import qualified Language.Ltml.ToLaTeX.GlobalState as GS
import Language.Ltml.ToLaTeX.PreLaTeXType
    ( PreLaTeX (ISequence, IText, MissingRef)
    , bold
    , enumerate
    , footnote
    , footref
    , hrule
    , hyperlink
    , hypertarget
    , label
    , linebreak
    , newpage
    , resetfootnote
    , setpdftitle
    )
import Text.Megaparsec (errorBundlePretty)

-- | class to convert AST objects into PreLaTeX. To be able to automatically generate
--   numbers and context for each object, the class function toPreLaTeXM works
--   in a State monad.
class ToPreLaTeXM a where
    toPreLaTeXM :: a -> State GS.GlobalState PreLaTeX

-- | helper class to pass labels from the AST object Node to its children
class Labelable a where
    attachLabel :: Maybe Label -> a -> State GS.GlobalState PreLaTeX

-------------------------------- Void -----------------------------------

instance ToPreLaTeXM Void where
    toPreLaTeXM = absurd

-------------------------------- [] -----------------------------------

instance (ToPreLaTeXM a) => ToPreLaTeXM [a] where
    toPreLaTeXM content = do
        content' <- mapM toPreLaTeXM content
        pure $ ISequence content'

-------------------------------- Maybe -----------------------------------

instance (ToPreLaTeXM a) => ToPreLaTeXM (Maybe a) where
    toPreLaTeXM Nothing = pure mempty
    toPreLaTeXM (Just content) = do
        toPreLaTeXM content

------------------------------- Flagged ----------------------------------

-- | This instance manages which part of the AST is actually translated into LaTeX;
--   Everything else is just used to build up the needed context (labels, etc.)
instance (ToPreLaTeXM a) => ToPreLaTeXM (Flagged' a) where
    toPreLaTeXM (Flagged b content) = do
        -- \| first check whether the global render flag (flaggedParent) is set
        b0 <- use (GS.flagState . GS.flaggedParent)
        -- \| set the scope for the content
        (GS.flagState . GS.flaggedParent) .= (b || b0)
        (GS.flagState . GS.flaggedChildren) .= False
        -- \| run the state to build the globalstate and get the potential result
        res <- toPreLaTeXM content
        -- \| reset the scope
        (GS.flagState . GS.flaggedParent) .= b0
        isParent <- use (GS.flagState . GS.flaggedChildren)
        (GS.flagState . GS.flaggedChildren) .= True
        -- \| return the result if needed
        if not isParent && not (b || b0)
            then pure mempty
            else pure res

-------------------------------- Node -----------------------------------

-- | the provided label is passed to the content of the node using
--   the helper class Labelable
instance (Labelable a) => ToPreLaTeXM (Node a) where
    toPreLaTeXM (Node mLabel content) = attachLabel mLabel content

-------------------------------- Text -----------------------------------

instance
    ( Stylable style
    , ToPreLaTeXM lbrk
    , ToPreLaTeXM fnref
    , ToPreLaTeXM enum
    , ToPreLaTeXM special
    )
    => ToPreLaTeXM (TextTree lbrk fnref style enum special)
    where
    toPreLaTeXM (Word t) = pure $ IText t
    toPreLaTeXM Space = pure $ IText " "
    toPreLaTeXM NonBreakingSpace = pure $ IText "\xA0"
    toPreLaTeXM (LineBreak lbrk) = toPreLaTeXM lbrk
    toPreLaTeXM (Special s) = toPreLaTeXM s
    toPreLaTeXM (Reference l) = pure $ MissingRef l
    toPreLaTeXM (Styled style tt) = do
        tt' <- toPreLaTeXM tt
        pure $ applyTextStyle style tt'
    toPreLaTeXM (Enum enum) = toPreLaTeXM enum
    toPreLaTeXM (FootnoteRef fnref) = toPreLaTeXM fnref

instance ToPreLaTeXM HardLineBreak where
    toPreLaTeXM HardLineBreak = pure linebreak

-- | Inserts footnotes depending on context
instance ToPreLaTeXM FootnoteReference where
    toPreLaTeXM (FootnoteReference l@(Label lt)) = do
        labelToRef <- use GS.labelToRef
        -- \| first check whether a footnote has already been seen
        case Map.lookup l labelToRef of
            Nothing -> do
                -- \| if the footnote has not been seen already then increase the counter,
                --                    insert the label into the map for normal refs and lookup the content
                --                    in the footnote map.
                --
                n <- GS.nextFootnote
                GS.insertRefLabel (Just l) (T.pack $ show n)
                labelToFootNote <- use GS.labelToFootNote
                case Map.lookup l labelToFootNote of
                    -- \| TODO: maybe throw error if the footnote doesnt
                    --   actually exist? shouldnt happen in reality,
                    --   otherwise the parser is broken
                    Nothing -> pure mempty
                    -- \| otherwise just get the content of the footnote
                    --   and add it to the output.
                    Just (Footnote _ tt) -> do
                        tt' <- toPreLaTeXM tt
                        pure $
                            footnote $
                                hypertarget l mempty
                                    <> label lt
                                    <> tt'
            Just _ -> pure $ footref lt

-- \| if the footnote has been seen then just insert a
--                    superscript number, not the content again.
--

-- | creates an enumeration in latex. the format of the enumerations is passed
--   to the children via the state.
instance ToPreLaTeXM Enumeration where
    toPreLaTeXM
        ( Enumeration
                (EnumFormat (EnumItemFormat ident (EnumItemKeyFormat key)))
                enumItems
            ) = do
            currentIdent <- use (GS.formatState . GS.enumIdentifierFormat)
            (GS.formatState . GS.enumIdentifierFormat) .= ident
            enumItems' <- mapM toPreLaTeXM enumItems
            (GS.formatState . GS.enumIdentifierFormat) .= currentIdent
            pure $ enumerate [getEnumStyle ident key] enumItems'

instance ToPreLaTeXM EnumItem where
    toPreLaTeXM = attachLabel Nothing

-- | the current path to the item is saved in the enumPosition in the state.
--   this is mainly needed for the label though, as the format itself is managed
--   by the enumeration environment
instance Labelable EnumItem where
    attachLabel mLabel (EnumItem tt) = do
        path <- GS.nextEnumPosition
        ident <- use (GS.formatState . GS.enumIdentifierFormat)
        GS.insertRefLabel mLabel (getIdentifier ident (last path))
        tt' <- GS.descendEnumTree $ toPreLaTeXM tt
        let anchor = maybe mempty (`hypertarget` mempty) mLabel
        pure $ anchor <> tt'

-- | way to label a sentence
instance ToPreLaTeXM SentenceStart where
    toPreLaTeXM (SentenceStart mLabel) = do
        n <- GS.nextSentence
        GS.insertRefLabel mLabel (T.pack (show n))
        maybe (pure mempty) toPreLaTeXM mLabel

-------------------------------- Label -----------------------------------

instance ToPreLaTeXM Label where
    toPreLaTeXM l = pure $ hyperlink l mempty

-------------------------------- Paragraph -----------------------------------

instance ToPreLaTeXM SimpleParagraph where
    toPreLaTeXM (SimpleParagraph (SimpleParagraphFormat t) content) = do
        content' <- toPreLaTeXM content
        pure $ applyTextStyle t content'

instance ToPreLaTeXM Paragraph where
    toPreLaTeXM = attachLabel Nothing

-- | paragraphs are rendered in an enumeration environment. therefor the current number is
--   set as a starting value. (there will not be more than one item)
--   could have been cleaner if the format of the paragraph was given to the LeafSectionBody.
instance Labelable Paragraph where
    attachLabel mLabel (Paragraph (ParagraphFormat ident (ParagraphKeyFormat key)) content) = do
        n <- GS.nextParagraph
        let identifier = getIdentifier ident n
        GS.insertRefLabel mLabel identifier
        GS.enumPosition .= [0]
        content' <- toPreLaTeXM content
        let anchor = maybe mempty (`hypertarget` mempty) mLabel
        -- \^ attaches the label if it exists
        b <- use (GS.flagState . GS.onlyOneParagraph)
        -- \^ not indented or numbered if there is only one paragraph
        pure $
            anchor
                <> if b
                    then content'
                    else
                        enumerate
                            [ T.pack $ "start=" <> show n
                            , getEnumStyle ident key
                            ]
                            [content']

--------------------------------- Table ------------------------------------

instance ToPreLaTeXM Table where
    toPreLaTeXM Table = undefined -- TODO

-------------------------------- Section -----------------------------------

instance ToPreLaTeXM SimpleSection where
    toPreLaTeXM (SimpleSection (SimpleSectionFormat hasHLine) content) = do
        content' <- toPreLaTeXM content
        pure $ (if hasHLine then hrule else mempty) <> content'

-- | there is no instance for the Headingtype, as it would be complicated to manage
--   regarding there are different situations in which a heading is used and moving
--   all the context via the state or a helper class or some other way would be tedious.
--   consequently we use a function that just builds the heading with the needed information.
createHeading
    :: HeadingFormat b -> PreLaTeX -> PreLaTeX -> State GS.GlobalState PreLaTeX
createHeading (HeadingFormat t hfmt) tt ident = do
    pure $
        applyTextStyle t $
            formatHeading hfmt ident tt

instance (ToPreLaTeXM a) => ToPreLaTeXM (SectionFormatted (Parsed a)) where
    toPreLaTeXM
        (SectionFormatted fmt s) =
            case s of
                Left e -> error (errorBundlePretty e)
                Right content -> do
                    (GS.formatState . GS.sectionFormat) .= fmt
                    toPreLaTeXM content

instance ToPreLaTeXM Section where
    toPreLaTeXM = attachLabel Nothing

-- | one of the trickier instances. since every part of the SectionBody needs the provided
--   context of the section it would be difficult to write a seperate SectionBody instance.
--   hence this instance got bloated a little
instance Labelable Section where
    attachLabel mLabel (Section h nodes) =
        case h of
            -- \^ if the parsing of the heading failed, we throw an error, otherwise proceed
            Left e -> error (errorBundlePretty e)
            Right (Heading fmt tt) -> do
                (SectionFormat ident (TocKeyFormat keyident)) <-
                    use (GS.formatState . GS.sectionFormat)
                tt' <- toPreLaTeXM tt
                -- \| helper functions to reduce redundance
                let headingText = tt'
                    buildHeading n = do
                        createHeading fmt headingText (IText $ getIdentifier ident n)
                    setLabel n = GS.insertRefLabel mLabel (T.pack (show n))
                    filterFN xs = [y | y <- xs, not (isFN y)]
                      where
                        -- \^ when the heading is added to the toc, we want to get rid of footnotes. (luckily Styled is not allowed in HeadingTextTree)

                        isFN (FootnoteRef _) = True
                        isFN _ = False
                tocEntry <- toPreLaTeXM $ filterFN tt
                case nodes of
                    LeafSectionBody paragraphs -> do
                        n <- GS.nextSection
                        setLabel n
                        GS.flagState . GS.onlyOneParagraph .= (length paragraphs == 1)
                        tocAnchor <- GS.addTOCEntry n keyident ident tocEntry
                        headingDoc <- buildHeading n
                        content' <- toPreLaTeXM paragraphs
                        let refAnchor = maybe headingDoc (`hypertarget` headingDoc) mLabel
                        pure $ tocAnchor <> refAnchor <> content'
                    InnerSectionBody subsections -> do
                        n <- GS.nextSupersection
                        setLabel n
                        GS.counterState . GS.supersectionCTR .= 0
                        tocAnchor <- GS.addTOCEntry n keyident ident tocEntry
                        headingDoc <- buildHeading n
                        content' <- toPreLaTeXM subsections
                        GS.counterState . GS.supersectionCTR .= n
                        let refAnchor =
                                maybe (headingDoc <> linebreak) (`hypertarget` (headingDoc <> linebreak)) mLabel
                        pure $ tocAnchor <> refAnchor <> content'
                    SimpleLeafSectionBody simpleblocks -> do
                        toPreLaTeXM simpleblocks

-------------------------------- Block ----------------------------------

instance ToPreLaTeXM SimpleBlock where
    toPreLaTeXM (SimpleParagraphBlock b) = toPreLaTeXM b
    toPreLaTeXM (TableBlock b) = toPreLaTeXM b

-------------------------------- Document -----------------------------------

instance ToPreLaTeXM Document where
    toPreLaTeXM = attachLabel Nothing

instance Labelable Document where
    attachLabel
        mLabel
        ( Document
                (DocumentFormat mTOC)
                dh
                (DocumentBody intro (Flagged b (NavTocHeaded _ content)) outro)
                footnotemap
            ) =
            case dh of
                Left e -> error (errorBundlePretty e)
                Right (DocumentHeading tt) -> do
                    -- \| build the heading text from the given HeadingFormat
                    --                    passed by the state and depending on the position we are in
                    tt' <- toPreLaTeXM tt
                    headingText <- buildHeading tt'

                    -- \| prepare the state for this document
                    GS.labelToFootNote .= footnotemap
                    GS.resetCountersSoft
                    GS.toc .= mempty

                    -- \| recursively receive the needed parts of the document
                    intro' <- toPreLaTeXM intro
                    content' <- case content of
                        Left e -> error (errorBundlePretty e)
                        Right section -> case section of
                            (LeafSectionBody paragraphs) -> do
                                toPreLaTeXM (Flagged b paragraphs)
                            (SimpleLeafSectionBody simpleblocks) -> do
                                toPreLaTeXM (Flagged b simpleblocks)
                            (InnerSectionBody sections) -> do
                                toPreLaTeXM (Flagged b sections)
                    outro' <- toPreLaTeXM outro

                    -- \| if we need a toc then we assemble it.
                    toc' <- case mTOC of
                        Nothing -> pure mempty
                        Just (TocFormat (TocHeading tocHeading)) -> buildTOC tocHeading

                    isFlagged <- use (GS.flagState . GS.flaggedParent)

                    preamble <- if isFlagged then pure $ headingText <> toc' else pure mempty

                    -- \| assemble the final document
                    pure $
                        resetfootnote
                            -- \^ since footnotes are document scoped, we need to reset them in latex as well
                            <> preamble
                            <> intro'
                            <> content'
                            <> outro'
          where
            -- \| helper function to create the heading depending on context
            buildHeading :: PreLaTeX -> State GS.GlobalState PreLaTeX
            buildHeading tt' = do
                docType <- use (GS.flagState . GS.docType)
                case docType of
                    GS.Appendix -> do
                        n <- GS.nextAppendix
                        AppendixElementFormat ident (TocKeyFormat key) fmt <-
                            use (GS.formatState . GS.appendixFormat)
                        let iText = getIdentifier ident n
                        GS.insertRefLabel mLabel iText
                        tocAnchor <- GS.addTOCEntry n key ident tt'
                        -- \^ inserts a toc entry and returns a hypertarget, so that it is possible to jump here from the toc
                        GS.addAppendixHeaderEntry n key ident tt'
                        heading <- createHeading fmt tt' (IText iText)
                        pure $ tocAnchor <> heading
                    GS.Main -> do
                        fmt <- use (GS.formatState . GS.docHeadingFormat)
                        createHeading fmt tt' (IText " ")

            -- \| helper function to render the toc
            buildTOC :: Text -> State GS.GlobalState PreLaTeX
            buildTOC tocHeading = do
                t <- use (GS.flagState . GS.docType)
                toc' <- use GS.toc
                appendixHeaders' <- use GS.appendixHeaders
                pure $
                    bold (IText tocHeading) <> linebreak <> case t of
                        GS.Appendix ->
                            ISequence $ DList.toList toc'
                        GS.Main ->
                            ISequence $ DList.toList (toc' <> appendixHeaders')

-------------------------------- AppendixSection -----------------------------------

instance ToPreLaTeXM AppendixSection where
    toPreLaTeXM
        ( AppendixSection
                ( AppendixSectionFormat
                        (AppendixSectionTitle t)
                        elementFmt
                    )
                nodes
            ) = do
            if null nodes
                -- \^ mainly to prevent the title to appear in the main toc, if there are no appendices
                then pure mempty
                else do
                    GS.counterState . GS.appendixCTR .= 0
                    GS.flagState . GS.docType .= GS.Appendix
                    GS.formatState . GS.appendixFormat .= elementFmt
                    GS.appendixHeaders %= (<> DList.fromList [IText t, linebreak])
                    nodes' <- mapM toPreLaTeXM nodes
                    pure $ ISequence $ map (newpage <>) nodes'

-------------------------------- DocumentContainer -----------------------------------

instance ToPreLaTeXM DocumentContainer where
    toPreLaTeXM
        ( DocumentContainer
                ( DocumentContainerFormat
                        headerFmt
                        footerFmt
                        (MainDocumentFormat _ headingFmt)
                    )
                (NavTocHeaded _ dch)
                doc
                appendices
            ) = case dch of
            Left e -> error (errorBundlePretty e)
            Right (DocumentContainerHeader pdfTitle superTitle title date) -> do
                -- \| prepare the state
                GS.preDocument %= (<> setpdftitle pdfTitle)
                GS.addHeaderFooter headerFmt footerFmt superTitle title date
                GS.formatState . GS.docHeadingFormat .= headingFmt
                GS.resetCountersHard

                appendices' <- toPreLaTeXM appendices
                -- \^ appendices before main doc to gather the headings for the toc of main doc

                GS.flagState . GS.docType .= GS.Main
                doc' <- toPreLaTeXM doc

                -- \| assemble the final document container
                pure $ doc' <> appendices'

-------------------------------- NavTocHeaded -----------------------------------

-- | irrelevant for pdf generation
instance (ToPreLaTeXM a) => ToPreLaTeXM (NavTocHeaded (Parsed a)) where
    toPreLaTeXM (NavTocHeaded _ content) = case content of
        Left e -> error (errorBundlePretty e)
        Right c -> toPreLaTeXM c
