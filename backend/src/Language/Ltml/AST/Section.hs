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

data Section
    = Section
        SectionFormat
        (Parsed Heading)
        SectionBody
    deriving (Show)

data Heading
    = Heading
        InnerHeadingFormat
        [HeadingTextTree]
    deriving (Show)

data SectionBody
    = InnerSectionBody [Flagged' (Parsed (Node Section))]
    | LeafSectionBody [Node Paragraph]
    | SimpleLeafSectionBody [SimpleBlock]
    deriving (Show)
