module Language.Ltml.AST.Section (Section) where

import Data.Text (Text)
import Language.Lsd.AST.Section (SectionFormat)
import Language.Ltml.AST.Block (Block)
import Language.Ltml.AST.Node (Node)

-- sectionKind = Section
-- sectionType = Section SectionFormat
data Section
    = Section
        SectionFormat
        Heading
        [Node SectionChild]

-- TODO: Use PlainText (yet to be defined).
newtype Heading = Heading Text

data SectionChild
    = SubSection Section
    | Block Block
