{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ltml.ToLaTeX.ToLaTeXM
    (ToLaTeXM (..))
    where

import Data.Void (Void, absurd)
import Language.Ltml.AST.Text
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Paragraph (Paragraph (..))
import Language.Ltml.AST.Section (Heading (..), Section (..))
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.ToLaTeX.Type
import qualified Data.Text.Lazy as LT
import Control.Monad.State (State, MonadState (get), modify)
import qualified Language.Ltml.ToLaTeX.GlobalState as LS
import Language.Ltml.ToLaTeX.Format
import Language.Ltml.AST.Document (Document(..), DocumentBody(..), DocumentHeader(..))
import Language.Lsd.AST.Type.Document (DocumentFormat(..))


class ToLaTeXM a where
    toLaTeXM :: a -> State LS.GlobalState LaTeX

-------------------------------- Void -----------------------------------

instance ToLaTeXM Void where
    toLaTeXM = absurd

-------------------------------- Text -----------------------------------

instance (ToLaTeXM enum,
          ToLaTeXM special)
          => ToLaTeXM (TextTree FontStyle enum special) where

    toLaTeXM (Word t)              = pure $ Text $ LT.fromStrict t
    toLaTeXM Space                 = pure $ Text $ LT.pack " "
    toLaTeXM (Special s)           = toLaTeXM s
    toLaTeXM (Reference l)         = pure $ MissingRef l
    toLaTeXM (Styled style tt)     = do
            tt' <- mapM toLaTeXM tt
            pure $ applyFontStyle style tt'
    toLaTeXM (Enum enum)           = toLaTeXM enum
    toLaTeXM (Footnote tt)         = do
            tt' <- mapM toLaTeXM tt
            pure $ (footnote . Sequence) tt'

applyFontStyle :: FontStyle -> [LaTeX] -> LaTeX
applyFontStyle Bold       = bold . Sequence
applyFontStyle Italics    = italic . Sequence
applyFontStyle Underlined = underline . Sequence

instance (ToLaTeXM enum,
          ToLaTeXM special)
          => ToLaTeXM (TextTree Void enum special) where

    toLaTeXM (Word t)              = pure $ Text $ LT.fromStrict t
    toLaTeXM Space                 = pure $ Text $ LT.pack " "
    toLaTeXM (Special s)           = toLaTeXM s
    toLaTeXM (Reference l)         = pure $ MissingRef l
    toLaTeXM (Styled style _)      = absurd style
    toLaTeXM (Enum enum)           = toLaTeXM enum
    toLaTeXM (Footnote tt)         = do
            tt' <- mapM toLaTeXM tt
            pure $ (footnote . Sequence) tt'

instance ToLaTeXM Enumeration where

    toLaTeXM (Enumeration enumItems) = do
        enumItems' <- mapM toLaTeXM enumItems
        pure $ enumerate enumItems'

instance ToLaTeXM EnumItem where

    toLaTeXM = attachLabel Nothing

-- TODO: reimplement the labelling with a list to track the label of the enum.
instance Labelable EnumItem where

    attachLabel mLabel (EnumItem tt) = do
        tt' <- mapM toLaTeXM tt
        pure $ Sequence tt'

instance ToLaTeXM SentenceStart where

    toLaTeXM (SentenceStart mLabel) = do
        n <- LS.nextSentence
        LS.insertLabel mLabel (LT.pack (show n))
        maybe (pure mempty) toLaTeXM mLabel


-------------------------------- Label -----------------------------------

instance ToLaTeXM Label where

    toLaTeXM l = pure $ hyperlink l mempty

class ToLaTeXM a => Labelable a where

    attachLabel :: Maybe Label -> a -> State LS.GlobalState LaTeX


-------------------------------- Paragraph -----------------------------------

instance ToLaTeXM Paragraph where

    toLaTeXM = attachLabel Nothing

instance Labelable Paragraph where
    attachLabel mLabel (Paragraph fmt content) = do
        _ <- LS.nextParagraph
        st <- get
        _ <- LS.insertLabel mLabel ("§ "
                                 <> LT.pack (show (LS.section st))
                                 <> " Absatz "
                                 <> LT.pack (show (LS.paragraph st)))
        content' <- mapM toLaTeXM content
        let anchor = maybe mempty (`hypertarget` mempty) mLabel
        pure $ anchor <>
                if LS.onlyOneParagraph st
                then Sequence content' <> medskip
                else paragraph (formatParagraph fmt (LS.paragraph st)) (Sequence content') <> medskip

-------------------------------- Section -----------------------------------
instance ToLaTeXM Heading where

    toLaTeXM (Heading fmt tt) = do
        tt' <- mapM toLaTeXM tt
        st <- get
        pure $ bold (formatHeading fmt
                        (LS.identifier st)
                        (Sequence tt'))

instance ToLaTeXM Section where

  toLaTeXM = attachLabel Nothing


instance Labelable Section where

    attachLabel mLabel (Section fmt heading nodes) = do
        (children, headingDoc) <-
            case nodes of
                Left paragraphs -> do -- | this is a section
                    n <- LS.nextSection
                    _ <- LS.insertLabel mLabel ("§ "
                                             <> LT.pack (show n))
                    modify (\s -> s { LS.identifier = formatSection fmt (LS.section s)
                                    , LS.onlyOneParagraph = length paragraphs == 1
                                    })
                    headingDoc <- toLaTeXM heading
                    children' <- mapM toLaTeXM paragraphs
                    pure (children', center [headingDoc])
                Right subsections -> do -- | this is a supersection
                    n <- LS.nextSupersection
                    _ <- LS.insertLabel mLabel ("Abschnitt "
                                             <> LT.pack (show n))
                    children' <- mapM toLaTeXM subsections
                    modify (\s -> s { LS.isSupersection = True
                                    , LS.identifier = formatSection fmt (LS.supersection s)
                                    })
                    headingDoc <- toLaTeXM heading
                    modify (\s -> s { LS.isSupersection = False })
                    pure (children', headingDoc <> linebreak)
        let anchor = maybe headingDoc (`hypertarget` headingDoc) mLabel
        pure $ anchor <> Sequence children

-------------------------------- Node -----------------------------------

instance Labelable a => ToLaTeXM (Node a) where

    toLaTeXM (Node mLabel a) = attachLabel mLabel a


-------------------------------- Document -----------------------------------

instance ToLaTeXM Document where

    toLaTeXM (Document DocumentFormat DocumentHeader (DocumentBody nodes)) = do
        nodes' <- mapM toLaTeXM nodes
        pure $ staticDocumentFormat <> document (Sequence nodes')

