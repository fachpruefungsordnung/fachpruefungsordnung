{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.ToPreLaTeXM (ToPreLaTeXM (..))
where

import Control.Lens (use, (%=), (.=), (.~))
import Control.Monad.State (State, modify)
import qualified Data.DList as DList
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LT
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
import Language.Lsd.AST.Type.Document (DocumentFormat (..))
import Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerFormat (DocumentContainerFormat)
    )
import Language.Lsd.AST.Type.Enum
    ( EnumFormat (..)
    , EnumItemFormat (EnumItemFormat)
    )
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (ParagraphFormat))
import Language.Lsd.AST.Type.Section (SectionFormat (SectionFormat))
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
import Language.Ltml.ToLaTeX.Format
    ( Stylable (..)
    , formatHeading
    , getEnumStyle
    , getIdentifier
    )
import qualified Language.Ltml.ToLaTeX.GlobalState as GS
import Language.Ltml.ToLaTeX.PreLaTeXType
    ( PreLaTeX (ISequence, IText, MissingRef)
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

class ToPreLaTeXM a where
    toPreLaTeXM :: a -> State GS.GlobalState PreLaTeX

class Labelable a where
    attachLabel :: Maybe Label -> a -> State GS.GlobalState PreLaTeX

-------------------------------- Void -----------------------------------

instance ToPreLaTeXM Void where
    toPreLaTeXM = absurd

-------------------------------- Node -----------------------------------

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
    toPreLaTeXM (Word t) = pure $ IText $ LT.fromStrict t
    toPreLaTeXM Space = pure $ IText $ LT.pack " "
    toPreLaTeXM NonBreakingSpace = pure $ IText $ LT.pack "\xA0"
    toPreLaTeXM (LineBreak lbrk) = toPreLaTeXM lbrk
    toPreLaTeXM (Special s) = toPreLaTeXM s
    toPreLaTeXM (Reference l) = pure $ MissingRef l
    toPreLaTeXM (Styled style tt) = do
        tt' <- mapM toPreLaTeXM tt
        pure $ applyTextStyle style (ISequence tt')
    toPreLaTeXM (Enum enum) = toPreLaTeXM enum
    toPreLaTeXM (FootnoteRef fnref) = toPreLaTeXM fnref

instance ToPreLaTeXM HardLineBreak where
    toPreLaTeXM HardLineBreak = pure linebreak

instance ToPreLaTeXM FootnoteReference where
    toPreLaTeXM (FootnoteReference l@(Label lt)) = do
        labelToRef <- use GS.labelToRef
        case Map.lookup l labelToRef of
            Nothing -> do
                n <- GS.nextFootnote
                GS.insertRefLabel (Just l) (LT.pack $ show n)
                labelToFootNote <- use GS.labelToFootNote
                case Map.lookup l labelToFootNote of
                    Nothing -> pure mempty -- TODO: maybe throw error here?
                    Just (Footnote _ tt) -> do
                        tt' <- mapM toPreLaTeXM tt
                        pure $
                            footnote $
                                hypertarget l mempty
                                    <> label (LT.fromStrict lt)
                                    <> ISequence tt'
            Just _ -> pure $ footref $ LT.fromStrict lt

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

instance Labelable EnumItem where
    attachLabel mLabel (EnumItem tt) = do
        path <- GS.nextEnumPosition
        ident <- use (GS.formatState . GS.enumIdentifierFormat)
        GS.insertRefLabel mLabel (getIdentifier ident (last path))
        tt' <- GS.descendEnumTree $ mapM toPreLaTeXM tt
        let anchor = maybe mempty (`hypertarget` mempty) mLabel
        pure $ anchor <> ISequence tt'

instance ToPreLaTeXM SentenceStart where
    toPreLaTeXM (SentenceStart mLabel) = do
        n <- GS.nextSentence
        GS.insertRefLabel mLabel (LT.pack (show n))
        maybe (pure mempty) toPreLaTeXM mLabel

-------------------------------- Label -----------------------------------

instance ToPreLaTeXM Label where
    toPreLaTeXM l = pure $ hyperlink l mempty

-------------------------------- Paragraph -----------------------------------

instance ToPreLaTeXM SimpleParagraph where
    toPreLaTeXM (SimpleParagraph (SimpleParagraphFormat t) content) = do
        content' <- mapM toPreLaTeXM content
        pure $
            applyTextStyle t $
                ISequence content'

instance ToPreLaTeXM Paragraph where
    toPreLaTeXM = attachLabel Nothing

instance Labelable Paragraph where
    attachLabel mLabel (Paragraph (ParagraphFormat ident (ParagraphKeyFormat key)) content) = do
        n <- GS.nextParagraph
        let identifier = getIdentifier ident n
        GS.insertRefLabel mLabel identifier
        GS.enumPosition .= [0]
        content' <- mapM toPreLaTeXM content
        let anchor = maybe mempty (`hypertarget` mempty) mLabel
        b <- use (GS.flagState . GS.onlyOneParagraph)
        pure $
            anchor
                <> if b
                    then ISequence content'
                    else
                        enumerate
                            [ LT.pack $ "start=" <> show n
                            , getEnumStyle ident key
                            ]
                            [ISequence content']

--------------------------------- Table ------------------------------------

instance ToPreLaTeXM Table where
    toPreLaTeXM Table = undefined -- TODO

-------------------------------- Section -----------------------------------

instance ToPreLaTeXM SimpleSection where
    toPreLaTeXM (SimpleSection (SimpleSectionFormat hasHLine) content) = do
        content' <- mapM toPreLaTeXM content
        pure $ (if hasHLine then hrule else mempty) <> ISequence content'

createHeading
    :: HeadingFormat b -> PreLaTeX -> PreLaTeX -> State GS.GlobalState PreLaTeX
createHeading (HeadingFormat t hfmt) tt ident = do
    pure $
        applyTextStyle t $
            formatHeading hfmt ident tt

instance ToPreLaTeXM Section where
    toPreLaTeXM = attachLabel Nothing

instance Labelable Section where
    attachLabel
        mLabel
        (Section (SectionFormat ident (TocKeyFormat keyident)) (Heading fmt tt) nodes) =
            do
                tt' <- mapM toPreLaTeXM tt
                let headingText = ISequence tt'
                    buildHeading n = do
                        createHeading fmt headingText (IText $ getIdentifier ident n)
                    setLabel n = GS.insertRefLabel mLabel (LT.pack (show n))
                case nodes of
                    LeafSectionBody paragraphs -> do
                        n <- GS.nextSection
                        setLabel n
                        GS.flagState . GS.onlyOneParagraph .= (length paragraphs == 1)
                        GS.addTOCEntry n keyident ident headingText
                        headingDoc <- buildHeading n
                        content' <- mapM toPreLaTeXM paragraphs
                        let anchor = maybe headingDoc (`hypertarget` headingDoc) mLabel
                        pure $ anchor <> ISequence content'
                    InnerSectionBody subsections -> do
                        n <- GS.nextSupersection
                        setLabel n
                        modify $
                            {-  -} (GS.flagState . GS.isSupersection .~ True)
                                . (GS.counterState . GS.supersectionCTR .~ 0)
                        GS.addTOCEntry n keyident ident headingText
                        headingDoc <- buildHeading n
                        content' <- mapM toPreLaTeXM subsections
                        modify $
                            {-  -} (GS.flagState . GS.isSupersection .~ False)
                                . (GS.counterState . GS.supersectionCTR .~ n)
                        let anchor =
                                maybe (headingDoc <> linebreak) (`hypertarget` (headingDoc <> linebreak)) mLabel
                        pure $ anchor <> ISequence content'
                    SimpleLeafSectionBody simpleblocks -> do
                        content' <- mapM toPreLaTeXM simpleblocks
                        pure $ ISequence content'

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
                (DocumentFormat hasTOC)
                (DocumentHeading tt)
                (DocumentBody intro content outro)
                footnotemap
            ) = do
            {- build the heading text from the given HeadingFormat
               passed by the state and depending on the position we are in -}
            tt' <- mapM toPreLaTeXM tt
            headingText <- buildHeading tt'

            {- prepare the state for this document -}
            GS.labelToFootNote .= footnotemap
            GS.resetCountersSoft
            GS.toc .= mempty

            {- recursively receive the needed parts of the document -}
            intro' <- mapM toPreLaTeXM intro
            content' <- case content of
                LeafSectionBody paragraphs -> do
                    mapM toPreLaTeXM paragraphs
                SimpleLeafSectionBody simpleblocks -> do
                    mapM toPreLaTeXM simpleblocks
                InnerSectionBody sections -> do
                    mapM toPreLaTeXM sections
            outro' <- mapM toPreLaTeXM outro

            {- if we need a toc then we assemble it. -}
            toc' <- buildTOC

            {- assemble the final document -}
            pure $
                headingText
                    <> ISequence toc'
                    <> ISequence intro'
                    <> ISequence content'
                    <> ISequence outro'
          where
            buildHeading :: [PreLaTeX] -> State GS.GlobalState PreLaTeX
            buildHeading tt' = do
                b <- use (GS.flagState . GS.docType)
                case b of
                    GS.Appendix -> do
                        n <- GS.nextAppendix
                        AppendixElementFormat ident (TocKeyFormat key) fmt <-
                            use (GS.formatState . GS.appendixFormat)
                        let iText = getIdentifier ident n
                        GS.insertRefLabel mLabel iText
                        GS.addTOCEntry n key ident (ISequence tt')
                        GS.addAppendixHeaderEntry n key ident (ISequence tt')
                        createHeading fmt (ISequence tt') (IText iText)
                    GS.Main -> do
                        fmt <- use (GS.formatState . GS.docHeadingFormat)
                        createHeading fmt (ISequence tt') (IText " ")

            buildTOC :: State GS.GlobalState [PreLaTeX]
            buildTOC
                | not hasTOC = pure mempty
                | otherwise = do
                    t <- use (GS.flagState . GS.docType)
                    case t of
                        GS.Main -> do
                            toc' <- use GS.toc
                            appendixHeaders' <- use GS.appendixHeaders
                            pure $ DList.toList (toc' <> appendixHeaders')
                        GS.Appendix -> do
                            toc' <- use GS.toc
                            pure $ DList.toList toc'

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
            GS.counterState . GS.appendixCTR .= 0
            GS.flagState . GS.docType .= GS.Appendix
            GS.formatState . GS.appendixFormat .= elementFmt
            GS.appendixHeaders %= (<> DList.fromList [IText (LT.fromStrict t), linebreak])
            nodes' <- mapM toPreLaTeXM nodes
            pure $ ISequence $ map ((newpage <> resetfootnote) <>) nodes'

instance ToPreLaTeXM DocumentContainer where
    toPreLaTeXM
        ( DocumentContainer
                (DocumentContainerFormat headerFmt footerFmt headingFmt)
                (DocumentContainerHeader pdfTitle superTitle title date)
                doc
                appendices
            ) = do
            {- prepare the state -}
            GS.preDocument %= (<> setpdftitle (LT.fromStrict pdfTitle))
            GS.addHeaderFooter headerFmt footerFmt superTitle title date
            GS.formatState . GS.docHeadingFormat .= headingFmt
            GS.resetCountersHard

            appendices' <- mapM toPreLaTeXM appendices

            GS.flagState . GS.docType .= GS.Main
            doc' <- toPreLaTeXM doc

            {- assemble the final document container -}
            pure $ doc' <> ISequence appendices'
