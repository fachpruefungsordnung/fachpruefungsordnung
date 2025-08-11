module Language.Ltml.AST.Section
    ( Section (..)
    , Heading (..)
    )
where

import Language.Lsd.AST.Format (HeadingFormat)
import Language.Lsd.AST.Type.Section (SectionFormat)
import Language.Ltml.AST.Node (Node)
import Language.Ltml.AST.Paragraph (Paragraph)
import Language.Ltml.AST.Text (PlainTextTree)

data Section
    = -- | Section
      Section
        SectionFormat
        (Maybe Heading)
        (Either [Node Paragraph] [Node Section])
        -- ^ children
    deriving (Show)

data Heading
    = Heading
        HeadingFormat
        [PlainTextTree]
    deriving (Show)
