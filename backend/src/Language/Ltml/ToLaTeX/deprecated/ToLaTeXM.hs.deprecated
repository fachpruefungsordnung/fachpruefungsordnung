{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.ToLaTeXM (ToLaTeXM (..))
where

import Control.Monad.State (MonadState (get), State, modify)
import qualified Data.Map as Map
import qualified Data.Text.Lazy as LT
import Data.Void (Void, absurd)
import Language.Lsd.AST.Format
    ( EnumItemKeyFormat (EnumItemKeyFormat)
    , HeadingFormat
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
    , DocumentTitle (..)
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
import Language.Ltml.ToLaTeX.Format
import qualified Language.Ltml.ToLaTeX.GlobalState as LS
import Language.Ltml.ToLaTeX.Type

class ToLaTeXM a where
    toLaTeXM :: a -> State LS.GlobalState LaTeX

class Labelable a where
    attachLabel :: Maybe Label -> a -> State LS.GlobalState LaTeX

-------------------------------- Void -----------------------------------

instance ToLaTeXM Void where
    toLaTeXM = absurd

-------------------------------- Node -----------------------------------

instance (Labelable a) => ToLaTeXM (Node a) where
    toLaTeXM (Node mLabel content) = attachLabel mLabel content

-------------------------------- Text -----------------------------------

instance
    ( Stylable style
    , ToLaTeXM fnref
    , ToLaTeXM enum
    , ToLaTeXM special
    )
    => ToLaTeXM (TextTree fnref style enum special)
    where
    toLaTeXM (Word t) = pure $ Text $ LT.fromStrict t
    toLaTeXM Space = pure $ Text $ LT.pack " "
    toLaTeXM (Special s) = toLaTeXM s
    toLaTeXM (Reference l) = pure $ MissingRef l
    toLaTeXM (Styled style tt) = do
        tt' <- mapM toLaTeXM tt
        pure $ applyTextStyle style (Sequence tt')
    toLaTeXM (Enum enum) = toLaTeXM enum
    toLaTeXM (FootnoteRef fnref) = toLaTeXM fnref

instance ToLaTeXM FootnoteReference where
    toLaTeXM (FootnoteReference l@(Label lt)) = do
        st <- get
        let mRef = Map.lookup l (LS.labelToRef st)
        case mRef of
            Nothing -> do
                n <- LS.nextFootnote
                LS.insertRefLabel (Just l) (LT.pack $ show n)
                let mFootnote = Map.lookup l (LS.labelToFootNote st)
                case mFootnote of
                    Nothing -> pure mempty -- TODO: maybe throw error here?
                    Just (Footnote _ tt) -> do
                        tt' <- mapM toLaTeXM tt
                        pure $
                            footnote $
                                hypertarget l mempty
                                    <> label (LT.fromStrict lt)
                                    <> Sequence tt'
            Just _ -> pure $ footref $ LT.fromStrict lt

instance ToLaTeXM Enumeration where
    toLaTeXM
        ( Enumeration
                (EnumFormat (EnumItemFormat ident (EnumItemKeyFormat key)))
                enumItems
            ) = do
            st <- get
            let currentIdent = LS.enumIdentifier st
            modify (\s -> s {LS.enumIdentifier = ident})
            enumItems' <- mapM toLaTeXM enumItems
            modify (\s -> s {LS.enumIdentifier = currentIdent})
            pure $ enumerate [getEnumStyle ident key] enumItems'

instance ToLaTeXM EnumItem where
    toLaTeXM = attachLabel Nothing

instance Labelable EnumItem where
    attachLabel mLabel (EnumItem tt) = do
        st <- get
        path <- LS.nextEnumPosition
        LS.insertRefLabel mLabel (getIdentifier (LS.enumIdentifier st) (last path))
        tt' <- LS.descendEnumTree $ mapM toLaTeXM tt
        let anchor = maybe mempty (`hypertarget` mempty) mLabel
        pure $ anchor <> Sequence tt'

instance ToLaTeXM SentenceStart where
    toLaTeXM (SentenceStart mLabel) = do
        n <- LS.nextSentence
        LS.insertRefLabel mLabel (LT.pack (show n))
        maybe (pure mempty) toLaTeXM mLabel

-------------------------------- Label -----------------------------------

instance ToLaTeXM Label where
    toLaTeXM l = pure $ hyperlink l mempty

-------------------------------- Paragraph -----------------------------------

instance ToLaTeXM SimpleParagraph where
    toLaTeXM (SimpleParagraph (SimpleParagraphFormat alignment fontsize) children) = do
        children' <- mapM toLaTeXM children
        pure $
            applyTextStyle alignment $
                applyTextStyle fontsize $
                    Sequence children'

instance ToLaTeXM Paragraph where
    toLaTeXM = attachLabel Nothing

instance Labelable Paragraph where
    attachLabel mLabel (Paragraph (ParagraphFormat ident (ParagraphKeyFormat key)) content) = do
        n <- LS.nextParagraph
        st <- get
        let identifier = getIdentifier ident n
        LS.insertRefLabel mLabel identifier
        modify (\s -> s {LS.enumPosition = [0]})
        content' <- mapM toLaTeXM content
        let anchor = maybe mempty (`hypertarget` mempty) mLabel
        pure $
            anchor
                <> if LS.onlyOneParagraph st
                    then Sequence content' <> medskip
                    else
                        enumerate
                            [ LT.pack $ "start=" <> show n
                            , getEnumStyle ident key
                            ]
                            [Sequence content']

--------------------------------- Table ------------------------------------

instance ToLaTeXM Table where
    toLaTeXM Table = undefined -- TODO

-------------------------------- Section -----------------------------------

instance ToLaTeXM SimpleSection where
    toLaTeXM (SimpleSection SimpleSectionFormat children) = do
        children' <- mapM toLaTeXM children
        pure $ Sequence children'

createHeading :: HeadingFormat -> LaTeX -> LaTeX -> State LS.GlobalState LaTeX
createHeading fmt tt ident = do
    pure $ bold $ formatHeading fmt ident tt

instance ToLaTeXM Section where
    toLaTeXM = attachLabel Nothing

instance Labelable Section where
    attachLabel
        mLabel
        (Section (SectionFormat ident (TocKeyFormat keyident)) (Heading fmt tt) nodes) =
            do
                tt' <- mapM toLaTeXM tt
                let headingText = Sequence tt'
                    buildHeading n = do
                        createHeading fmt headingText (Text $ getIdentifier ident n)
                    setLabel n = LS.insertRefLabel mLabel (LT.pack (show n))
                case nodes of
                    LeafSectionBody paragraphs -> do
                        n <- LS.nextSection
                        setLabel n
                        modify (\s -> s {LS.onlyOneParagraph = length paragraphs == 1})
                        LS.addTOCEntry n keyident ident headingText
                        headingDoc <- buildHeading n
                        children' <- mapM toLaTeXM paragraphs
                        let anchor = maybe (center [headingDoc]) (`hypertarget` center [headingDoc]) mLabel
                        pure $ anchor <> Sequence children'
                    InnerSectionBody subsections -> do
                        n <- LS.nextSupersection
                        setLabel n
                        modify
                            ( \s ->
                                s
                                    { LS.isSupersection = True
                                    , LS.supersectionCTR = 0
                                    }
                            )
                        LS.addTOCEntry n keyident ident headingText
                        headingDoc <- buildHeading n
                        children' <- mapM toLaTeXM subsections
                        modify
                            ( \s ->
                                s
                                    { LS.isSupersection = False
                                    , LS.supersectionCTR = n
                                    }
                            )
                        let anchor =
                                maybe (headingDoc <> linebreak) (`hypertarget` (headingDoc <> linebreak)) mLabel
                        pure $ anchor <> Sequence children'
                    SimpleLeafSectionBody simpleblocks -> do
                        children' <- mapM toLaTeXM simpleblocks
                        pure $ Sequence children'

-------------------------------- Block ----------------------------------

instance ToLaTeXM SimpleBlock where
    toLaTeXM (SimpleParagraphBlock b) = toLaTeXM b
    toLaTeXM (TableBlock b) = toLaTeXM b

-------------------------------- Document -----------------------------------

instance ToLaTeXM Document where
    toLaTeXM = attachLabel Nothing

instance Labelable Document where
    attachLabel
        mLabel
        ( Document
                DocumentFormat
                (DocumentTitle t)
                (DocumentBody intro content outro)
                footnotemap
            ) = do
            st <- get
            headingText <-
                if LS.isAppendix st
                    then do
                        n <- LS.nextAppendix
                        let AppendixElementFormat ident (TocKeyFormat key) fmt = LS.appendixFormat st
                            iText = getIdentifier ident n
                        LS.insertRefLabel mLabel iText
                        LS.addTOCEntry n key ident (Text $ LT.fromStrict t)
                        createHeading fmt (Text $ LT.fromStrict t) (Text iText)
                    else do pure $ Text (LT.fromStrict t)
            modify
                ( \s ->
                    s
                        { LS.labelToFootNote = footnotemap
                        , LS.supersectionCTR = 0
                        , LS.sectionCTR = 0
                        , LS.paragraphCTR = 0
                        , LS.sentenceCTR = 0
                        }
                )
            intro' <- mapM toLaTeXM intro
            content' <- case content of
                LeafSectionBody paragraphs -> do
                    mapM toLaTeXM paragraphs
                SimpleLeafSectionBody simpleblocks -> do
                    mapM toLaTeXM simpleblocks
                InnerSectionBody sections -> do
                    mapM toLaTeXM sections
            outro' <- mapM toLaTeXM outro
            pure $
                headingText
                    <> Sequence intro'
                    <> Sequence content'
                    <> Sequence outro'

instance ToLaTeXM AppendixSection where
    toLaTeXM
        ( AppendixSection
                ( AppendixSectionFormat
                        (AppendixSectionTitle t)
                        elementFmt
                    )
                nodes
            ) = do
            modify
                ( \s ->
                    s
                        { LS.appendixCTR = 0
                        , LS.isAppendix = True
                        , LS.appendixFormat = elementFmt
                        }
                )
            nodes' <- mapM toLaTeXM nodes
            modify (\s -> s {LS.isAppendix = False})
            pure $
                Text (LT.fromStrict t)
                    <> Sequence nodes'

instance ToLaTeXM DocumentContainer where
    toLaTeXM
        ( DocumentContainer
                DocumentContainerFormat
                DocumentContainerHeader
                doc
                appendices
            ) = do
            doc' <- toLaTeXM doc
            appendices' <- mapM toLaTeXM appendices
            pure $ doc' <> Sequence appendices'
