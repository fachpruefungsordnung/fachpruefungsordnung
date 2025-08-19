module Language.Lsd.AST.Type.Text
    ( TextType (..)
    , PreTextType (..)
    )
where

import Language.Lsd.AST.SimpleRegex (Disjunction)

newtype TextType enumT
    = TextType
        (Disjunction enumT)

newtype PreTextType enumT
    = PreTextType
        (Disjunction enumT)
