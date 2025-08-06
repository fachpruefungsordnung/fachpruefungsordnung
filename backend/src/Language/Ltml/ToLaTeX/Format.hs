{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Format (
    formatIdentifier,
    formatHeading,
    formatParagraph,
    formatSection,
    staticDocumentFormat
) where
import Language.Lsd.AST.Format 
import Language.Ltml.ToLaTeX.Type 
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (ParagraphFormat))
import Language.Lsd.AST.Type.Section (SectionFormat (SectionFormat))
import qualified Data.Text.Lazy as LT
import Data.Char (chr)


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

staticDocumentFormat :: LaTeX
staticDocumentFormat = 
    Sequence
        [
            documentclass [] "article",
            setfontArabic,
            usepackage [ "letterpaper"
                        , "top=2cm"
                        , "bottom=2cm"
                        , "left=3cm"
                        , "right=3cm"
                        , "marginparwidth=1.75cm"
                        ]
                        "geometry",
            usepackage [ "colorlinks=true"
                        , "allcolors=red"
                        ]
                        "hyperref",
            usepackage [] "enumitem",
            usepackage [] "tabularx",
            enumStyle,
            setindent
        ]