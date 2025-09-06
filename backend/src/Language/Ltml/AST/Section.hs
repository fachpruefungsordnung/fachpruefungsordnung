module Language.Ltml.AST.Section
    ( Section (..)
    , FormattedSection
    , Heading (..)
    , SectionBody (..)
    )
where

import Language.Lsd.AST.Format (InnerHeadingFormat)
import Language.Lsd.AST.Type.Section (SectionFormatted)
import Language.Ltml.AST.Node (Node)
import Language.Ltml.AST.Paragraph (Paragraph)
import Language.Ltml.AST.SimpleBlock (SimpleBlock)
import Language.Ltml.AST.Text (HeadingTextTree)
import Language.Ltml.Common (Flagged', Parsed)

type FormattedSection = SectionFormatted (Parsed (Node Section))

data Section
    = Section
        (Parsed Heading)
        SectionBody
    deriving (Show)

data Heading
    = Heading
        InnerHeadingFormat
        [HeadingTextTree]
    deriving (Show)

data SectionBody
    = InnerSectionBody [Flagged' FormattedSection]
    | LeafSectionBody [Node Paragraph]
    | SimpleLeafSectionBody [SimpleBlock]
    deriving (Show)
