module Language.Ltml.AST.Section
    ( Section (..)
    , Heading (..)
    , SectionBody (..)
    )
where

import Language.Lsd.AST.Format (InnerHeadingFormat)
import Language.Lsd.AST.Type.Section (SectionFormat)
import Language.Ltml.AST.Node (Node)
import Language.Ltml.AST.Paragraph (Paragraph)
import Language.Ltml.AST.SimpleBlock (SimpleBlock)
import Language.Ltml.AST.Text (HeadingTextTree)
import Language.Ltml.Common (Flagged', Parsed)

-- | Section.
--   Unlike the type suggests, either the heading or the whole section is
--   parsed from text; if parsing a whole section fails, the same error is
--   reported for both the heading and the body.
--   The type is defined as-is to ease its use by the output generators (HTML,
--   LaTeX).
data Section
    = Section
        SectionFormat
        (Parsed Heading)
        (Parsed SectionBody)
    deriving (Show)

data Heading
    = Heading
        InnerHeadingFormat
        [HeadingTextTree]
    deriving (Show)

data SectionBody
    = InnerSectionBody [Flagged' (Node Section)]
    | LeafSectionBody [Node Paragraph]
    | SimpleLeafSectionBody [SimpleBlock]
    deriving (Show)
