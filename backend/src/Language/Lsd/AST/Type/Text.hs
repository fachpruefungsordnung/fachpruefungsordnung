module Language.Lsd.AST.Type.Text
    ( TextType (..)
    , PreTextType (..)
    )
where

newtype TextType enumT
    = TextType
        [enumT]

newtype PreTextType enumT
    = PreTextType
        [enumT]
