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
import Control.Monad.State (State, MonadState (get, put))
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
    toLaTeXM (Reference (Label l)) = pure $ ref $ LT.fromStrict l
    toLaTeXM (Styled style tt)     = do
            tt' <- mapM toLaTeXM tt
            pure $ applyFontStyle style tt' 
    toLaTeXM (Enum enum)           = toLaTeXM enum
    toLaTeXM (Footnote tt)         = do 
            tt' <- mapM toLaTeXM tt
            pure $ footnote tt'

applyFontStyle :: FontStyle -> [LaTeX] -> LaTeX
applyFontStyle Bold       = bold
applyFontStyle Italics    = italic
applyFontStyle Underlined = underline

instance (ToLaTeXM enum, 
          ToLaTeXM special) 
          => ToLaTeXM (TextTree Void enum special) where

    toLaTeXM (Word t)              = pure $ Text $ LT.fromStrict t
    toLaTeXM Space                 = pure $ Text $ LT.pack " "
    toLaTeXM (Special s)           = toLaTeXM s
    toLaTeXM (Reference (Label l)) = pure $ ref $ LT.fromStrict l
    toLaTeXM (Styled style _)      = absurd style
    toLaTeXM (Enum enum)           = toLaTeXM enum
    toLaTeXM (Footnote tt)         = do 
            tt' <- mapM toLaTeXM tt
            pure $ footnote tt'

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

instance ToLaTeXM Label where

    toLaTeXM (Label t) = pure $ label $ LT.fromStrict t

class ToLaTeXM a => Labelable a where
    attachLabel :: Maybe Label -> a -> State LS.LabelState LaTeX


-------------------------------- Paragraph -----------------------------------

instance ToLaTeXM Paragraph where

    toLaTeXM = attachLabel Nothing

instance Labelable Paragraph where
    attachLabel mLabel (Paragraph fmt content) = do
        _ <- LS.nextParagraph
        st <- get
        content' <- mapM toLaTeXM content
        pure $ formatParagraph fmt (LS.paragraph st) (Sequence content')

-------------------------------- Section -----------------------------------
instance ToLaTeXM Heading where

    toLaTeXM (Heading fmt tt) = do 
        tt' <- mapM toLaTeXM tt
        st <- get
        pure $ formatHeading fmt 
                        (if isSupersection st 
                            then LS.supersection st 
                            else LS.section st) 
                        (Sequence tt')

instance ToLaTeXM Section where

  toLaTeXM = attachLabel Nothing


instance Labelable Section where
    attachLabel mLabel (Section fmt heading nodes) = do
        (children, headingDoc, sectionID) <- 
            case nodes of
                Left subsections -> do 
                    _ <- LS.nextSupersection
                    st <- get
                    children' <- mapM toLaTeXM subsections
                    put st { isSupersection = True }
                    headingDoc       <- toLaTeXM heading
                    put st { isSupersection = False }
                    pure (children', headingDoc, LS.supersection st)
                Right paragraphs -> do
                    _ <- LS.nextSection
                    st <- get
                    headingDoc       <- toLaTeXM heading
                    children' <- mapM toLaTeXM paragraphs
                    pure (children', headingDoc, LS.section st)

        pure $ headingDoc <> formatSection fmt sectionID (Sequence children)

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

formatIdentifier :: IdentifierFormat -> Int -> LaTeX -> LaTeX
formatIdentifier (FormatString []) _ _ = mempty
formatIdentifier (FormatString (StringAtom s:rest)) i latex = 
    Text (LT.pack s) <> formatIdentifier (FormatString rest) i latex
formatIdentifier (FormatString (PlaceholderAtom a:rest)) i latex = 
    ( <> formatIdentifier (FormatString rest) i latex ) $
    case a of 
        Arabic          -> Text (LT.pack $ show i)
        AlphabeticLower -> Text (LT.pack [chr (i `mod` 27 + 96)])
        AlphabeticUpper -> Text (LT.pack [chr (i `mod` 27 + 64)])
    

formatHeading :: HeadingFormat -> Int -> LaTeX -> LaTeX
formatHeading (FormatString []) _ _ = mempty
formatHeading (FormatString (StringAtom s:rest)) i latex = 
    Text (LT.pack s) <> formatHeading (FormatString rest) i latex
formatHeading (FormatString (PlaceholderAtom a:rest)) i latex = 
    case a of
        HeadingTextPlaceholder -> latex <> formatHeading (FormatString rest) i latex
        IdentifierPlaceholder  -> Text (LT.pack $ show i) <> formatHeading (FormatString rest) i latex


formatParagraph :: ParagraphFormat -> Int -> LaTeX -> LaTeX
formatParagraph (ParagraphFormat x) = formatIdentifier x

formatSection :: SectionFormat -> Int -> LaTeX -> LaTeX
formatSection (SectionFormat x) = formatIdentifier x

