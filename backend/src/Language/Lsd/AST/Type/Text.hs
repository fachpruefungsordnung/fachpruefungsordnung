module Language.Lsd.AST.Type.Text
    ( TextType (..)
    )
where

import Language.Lsd.AST.SimpleRegex (Disjunction)
import Language.Lsd.AST.Type (NamedType)

newtype TextType enumT
    = TextType
        (Disjunction (NamedType enumT))
