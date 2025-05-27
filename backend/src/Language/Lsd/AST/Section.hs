module Language.Lsd.AST.Section
    ( SectionFormat
    )
where

import Language.Lsd.AST.Format (HeadingFormat, IdentifierFormat)

data SectionFormat
    = SectionFormat
        IdentifierFormat
        HeadingFormat
