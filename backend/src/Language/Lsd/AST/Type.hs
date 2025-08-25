-- | LTML types.
--
--   An LTML type is value of an LTML kind, which is a Haskell type
--   (e.g., 'Language.Lsd.AST.Type.Section.SectionType').
--
--   LTML types can be named, by wrapping them as (@'NamedType' t@), where @t@
--   is the specific type/kind.
module Language.Lsd.AST.Type
    ( NamedType (..)
    , unwrapNT
    )
where

import Language.Lsd.AST.Common (DisplayName, TypeName)

data NamedType t = NamedType TypeName DisplayName t

unwrapNT :: NamedType t -> t
unwrapNT (NamedType _ _ t) = t
