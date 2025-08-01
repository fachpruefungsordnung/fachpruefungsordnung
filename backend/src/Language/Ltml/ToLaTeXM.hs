{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ltml.ToLaTeXM
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
import qualified Language.Ltml.ToLaTeX.LabelState as LS
import Language.Lsd.AST.Format (FormatString (FormatString), FormatAtom (StringAtom, PlaceholderAtom), IdentifierFormat, HeadingFormat, HeadingPlaceholderAtom (HeadingTextPlaceholder, IdentifierPlaceholder), EnumStyle (Arabic, AlphabeticLower, AlphabeticUpper))
import Data.Char (chr)
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (ParagraphFormat))
import Language.Lsd.AST.Type.Section (SectionFormat (SectionFormat))
import Language.Ltml.ToLaTeX.LabelState (LabelState(isSupersection))


class ToLaTeXM a where
    toLaTeXM :: a -> State LS.LabelState LaTeX

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

    toLaTeXM (EnumItem tt) = do
        tt' <- mapM toLaTeXM tt
        pure $ Sequence tt'

instance ToLaTeXM SentenceStart where

    toLaTeXM (SentenceStart mLabel) = maybe (pure mempty) toLaTeXM mLabel

-------------------------------- Label -----------------------------------

-- | TODO: There could be an option to know where exactly we currently are, maybe by 
--         constantly updating the state, so that we can create a label from anywhere,
--         highly dependent on what SentenceStart labels are supposed to be.
instance ToLaTeXM Label where

    toLaTeXM l = pure $ hyperlink l mempty

class ToLaTeXM a => Labelable a where
    attachLabel :: Maybe Label -> a -> State LS.LabelState LaTeX


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
        pure $ anchor <> if LS.onlyOneParagraph st
                then Sequence content'
                else paragraph (formatParagraph fmt (LS.paragraph st)) (Sequence content')

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
                    modify (\s -> s { isSupersection = True
                                    , LS.identifier = formatSection fmt (LS.supersection s)
                                    })
                    headingDoc <- toLaTeXM heading
                    modify (\s -> s { isSupersection = False })
                    pure (children', headingDoc <> linebreak)
        let anchor = maybe headingDoc (`hypertarget` headingDoc) mLabel
        pure $ anchor <> Sequence children

-------------------------------- Node -----------------------------------

instance Labelable a => ToLaTeXM (Node a) where

    toLaTeXM (Node mLabel a) = attachLabel mLabel a


-------------------------------- Document -----------------------------------

-- instance ToLaTeXM Document where

--     toLaTeXM (Document _ _ (DocumentBody nodes)) = header <> T.intercalate "\n" (map toLaTeXM nodes) <> footer
--       where
--         header = "\\documentclass{article}\n"
--               <> "\\usepackage{enumitem}\n"
--               <> "% Define German legal style for enumerations\n"
--               <> "\\setlist[enumerate,1]{label=\\arabic*., left=0pt}\n"
--               <> "\\setlist[enumerate,2]{label=\\alph*), left=1.5em}\n"
--               <> "\\setlist[enumerate,3]{label=\\alph\\alph*), left=3em} \n"
--               <> "\\begin{document}\n"
--         footer = "\n\\end{document}"

-------------------------------- Format -----------------------------------

formatIdentifier :: IdentifierFormat -> Int -> LaTeX
formatIdentifier (FormatString []) _ = mempty
formatIdentifier (FormatString (StringAtom s:rest)) i =
    Text (LT.pack s) <> formatIdentifier (FormatString rest) i
formatIdentifier (FormatString (PlaceholderAtom a:rest)) i =
    ( <> formatIdentifier (FormatString rest) i ) $
    case a of
        Arabic          -> Text (LT.pack $ show i)
        AlphabeticLower -> Text (LT.pack [chr (i `mod` 27 + 96)])
        AlphabeticUpper -> Text (LT.pack [chr (i `mod` 27 + 64)])


formatHeading :: HeadingFormat -> LaTeX -> LaTeX -> LaTeX
formatHeading (FormatString []) _ _ = mempty
formatHeading (FormatString (StringAtom s:rest)) i latex =
    Sequence (map replace s) <> formatHeading (FormatString rest) i latex
  where
    replace '\n' = linebreak
    replace c    = Text (LT.pack [c])
formatHeading (FormatString (PlaceholderAtom a:rest)) i latex =
    case a of
        HeadingTextPlaceholder -> latex <> formatHeading (FormatString rest) i latex
        IdentifierPlaceholder  -> i <> formatHeading (FormatString rest) i latex


formatParagraph :: ParagraphFormat -> Int -> LaTeX
formatParagraph (ParagraphFormat x) = formatIdentifier x

formatSection :: SectionFormat -> Int  -> LaTeX
formatSection (SectionFormat x) = formatIdentifier x

