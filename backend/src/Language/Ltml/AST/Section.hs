module Language.Ltml.AST.Section
    ( Section (..)
    , Heading (..)
    , SectionBody (..)
    )
where

import Language.Lsd.AST.Format (HeadingFormat)
import Language.Lsd.AST.Type.Section (SectionFormat)
import Language.Ltml.AST.Node (Node)
import Language.Ltml.AST.Paragraph (Paragraph)
import Language.Ltml.AST.SimpleBlock (SimpleBlock)
import Language.Ltml.AST.Text (PlainTextTree)

data Section
    = Section
        SectionFormat
        Heading
        SectionBody
    deriving (Show)

data Heading
    = Heading
        HeadingFormat
        [PlainTextTree]
    deriving (Show)

data SectionBody
    = InnerSectionBody [Node Section]
    | LeafSectionBody [Node Paragraph]
    | SimpleLeafSectionBody [SimpleBlock]
    deriving (Show)
