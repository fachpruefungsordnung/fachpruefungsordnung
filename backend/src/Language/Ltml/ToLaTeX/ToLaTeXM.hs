{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.ToLaTeXM (ToLaTeXM (..))
where

import Control.Monad.State (MonadState (get), State, modify)
import qualified Data.Text.Lazy as LT
import Data.Void (Void, absurd)
import Language.Lsd.AST.Format
    ( HeadingFormat
    , ParagraphKeyFormat (ParagraphKeyFormat)
    , TocKeyFormat (TocKeyFormat)
    )
import Language.Lsd.AST.Type.Document (DocumentFormat (..))
import Language.Lsd.AST.Type.Enum
    ( EnumFormat (..)
    , EnumItemFormat (EnumItemFormat)
    )
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (ParagraphFormat))
import Language.Lsd.AST.Type.Section (SectionFormat (SectionFormat))
import Language.Ltml.AST.Document
    ( Document (..)
    , DocumentBody (..)
    , DocumentTitle (..)
    )
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.AST.Paragraph (Paragraph (..))
import Language.Ltml.AST.Section (Heading (..), Section (..))
import Language.Ltml.AST.Text
import Language.Ltml.ToLaTeX.Format
import qualified Language.Ltml.ToLaTeX.GlobalState as LS
import Language.Ltml.ToLaTeX.Type

class ToLaTeXM a where
    toLaTeXM :: a -> State LS.GlobalState LaTeX

-------------------------------- Void -----------------------------------

instance ToLaTeXM Void where
    toLaTeXM = absurd

-------------------------------- Text -----------------------------------

instance
    ( ToLaTeXM enum
    , ToLaTeXM special
    )
    => ToLaTeXM (TextTree FontStyle enum special)
    where
    toLaTeXM (Word t) = pure $ Text $ LT.fromStrict t
    toLaTeXM Space = pure $ Text $ LT.pack " "
    toLaTeXM (Special s) = toLaTeXM s
    toLaTeXM (Reference l) = pure $ MissingRef l
    toLaTeXM (Styled style tt) = do
        tt' <- mapM toLaTeXM tt
        pure $ applyFontStyle style tt'
    toLaTeXM (Enum enum) = toLaTeXM enum
    toLaTeXM (Footnote tt) = do
        tt' <- mapM toLaTeXM tt
        pure $ (footnote . Sequence) tt'

applyFontStyle :: FontStyle -> [LaTeX] -> LaTeX
applyFontStyle Bold = bold . Sequence
applyFontStyle Italics = italic . Sequence
applyFontStyle Underlined = underline . Sequence

instance
    ( ToLaTeXM enum
    , ToLaTeXM special
    )
    => ToLaTeXM (TextTree Void enum special)
    where
    toLaTeXM (Word t) = pure $ Text $ LT.fromStrict t
    toLaTeXM Space = pure $ Text $ LT.pack " "
    toLaTeXM (Special s) = toLaTeXM s
    toLaTeXM (Reference l) = pure $ MissingRef l
    toLaTeXM (Styled style _) = absurd style
    toLaTeXM (Enum enum) = toLaTeXM enum
    toLaTeXM (Footnote tt) = do
        tt' <- mapM toLaTeXM tt
        pure $ (footnote . Sequence) tt'

instance ToLaTeXM Enumeration where
    toLaTeXM (Enumeration fmt@(EnumFormat (EnumItemFormat ident _)) enumItems) = do
        st <- get
        let currentIdent = LS.enumIdentifier st
        modify (\s -> s {LS.enumIdentifier = ident})
        enumItems' <- mapM toLaTeXM enumItems
        modify (\s -> s {LS.enumIdentifier = currentIdent})
        pure $ enumerate [getEnumStyle fmt] enumItems'

instance ToLaTeXM EnumItem where
    toLaTeXM = attachLabel Nothing

instance Labelable EnumItem where
    attachLabel mLabel (EnumItem tt) = do
        st <- get
        path <- LS.nextEnumPosition
        LS.insertLabel mLabel (getIdentifier (LS.enumIdentifier st) (last path))
        tt' <- LS.descendEnumTree $ mapM toLaTeXM tt
        pure $ Sequence tt'

instance ToLaTeXM SentenceStart where
    toLaTeXM (SentenceStart mLabel) = do
        n <- LS.nextSentence
        LS.insertLabel mLabel (LT.pack (show n))
        maybe (pure mempty) toLaTeXM mLabel

-------------------------------- Label -----------------------------------

instance ToLaTeXM Label where
    toLaTeXM l = pure $ hyperlink l mempty

class (ToLaTeXM a) => Labelable a where
    attachLabel :: Maybe Label -> a -> State LS.GlobalState LaTeX

-------------------------------- Paragraph -----------------------------------

instance ToLaTeXM Paragraph where
    toLaTeXM = attachLabel Nothing

instance Labelable Paragraph where
    attachLabel mLabel (Paragraph (ParagraphFormat ident (ParagraphKeyFormat keyident)) content) = do
        n <- LS.nextParagraph
        st <- get
        let identifier = getIdentifier ident n
        LS.insertLabel mLabel identifier
        modify (\s -> s {LS.enumPosition = [0]})
        content' <- mapM toLaTeXM content
        let anchor = maybe mempty (`hypertarget` mempty) mLabel
        pure $
            anchor
                <> if LS.onlyOneParagraph st
                    then Sequence content' <> medskip
                    else
                        paragraph (formatKey keyident (Text identifier)) (Sequence content') <> medskip

-------------------------------- Section -----------------------------------

createHeading :: HeadingFormat -> LaTeX -> LaTeX -> State LS.GlobalState LaTeX
createHeading fmt tt ident = do
    pure $ bold $ formatHeading fmt ident tt

instance ToLaTeXM Section where
    toLaTeXM = attachLabel Nothing

instance Labelable Section where
    attachLabel mLabel (Section (SectionFormat ident (TocKeyFormat keyident)) (Heading fmt tt) nodes) = do
        tt' <- mapM toLaTeXM tt
        let headingText = Sequence tt'
            buildHeading n = do
                createHeading fmt headingText (Text $ getIdentifier ident n)
            setLabel n = LS.insertLabel mLabel (LT.pack (show n))
        case nodes of
            Left paragraphs -> do
                n <- LS.nextSection
                setLabel n
                modify
                    ( \s ->
                        s
                            { LS.onlyOneParagraph = length paragraphs == 1
                            , LS.toc =
                                LS.toc s
                                    <> LS.toDList
                                        [ formatKey keyident (Text $ getIdentifier ident n)
                                        , Text " "
                                        , headingText
                                        , linebreak
                                        ]
                            }
                    )
                headingDoc <- buildHeading n
                children' <- mapM toLaTeXM paragraphs
                let anchor = maybe (center [headingDoc]) (`hypertarget` center [headingDoc]) mLabel
                pure $ anchor <> Sequence children'
            Right subsections -> do
                n <- LS.nextSupersection
                setLabel n
                modify (\s -> s {LS.isSupersection = True})
                headingDoc <- buildHeading n
                children' <- mapM toLaTeXM subsections
                modify (\s -> s {LS.isSupersection = False})
                let anchor =
                        maybe (headingDoc <> linebreak) (`hypertarget` (headingDoc <> linebreak)) mLabel
                pure $ anchor <> Sequence children'

-------------------------------- Node -----------------------------------

instance (Labelable a) => ToLaTeXM (Node a) where
    toLaTeXM (Node mLabel a) = attachLabel mLabel a

-------------------------------- Document -----------------------------------

instance ToLaTeXM Document where
    toLaTeXM (Document DocumentFormat (DocumentTitle t) (DocumentBody nodes)) = do
        nodes' <- mapM toLaTeXM nodes
        pure $
            staticDocumentFormat <> document (Sequence $ Text (LT.fromStrict t) : nodes')
