{-# LANGUAGE ScopedTypeVariables #-}

-- | LTML types.
--
--   An LTML type is value of an LTML kind, which is a Haskell type
--   (e.g., 'Language.Lsd.AST.Type.Section.SectionType').
--
--   LTML types can be named, by wrapping them as (@'NamedType' t@), where @t@
--   is the specific type/kind.
--
--   A kind @t@ can be represented as Haskell value of type @'Proxy' t@.
module Language.Lsd.AST.Type
    ( NamedType (..)
    , KindNameOf (kindNameOf)
    , TypeNameOf (typeNameOf)
    )
where

import Data.Proxy (Proxy (Proxy))
import Language.Lsd.AST.Common (DisplayName, KindName, TypeName)

data NamedType t
    = NamedType
    { ntTypeName :: TypeName
    , ntDisplayName :: DisplayName
    , unwrapNT :: t
    }

class KindNameOf t where
    kindNameOf :: Proxy t -> KindName

instance (KindNameOf t) => KindNameOf (NamedType t) where
    kindNameOf _ = kindNameOf (Proxy :: Proxy t)

-- | Provider of standard names for pseudo-types; that is, such that are not
--   defined in an LSD, but rather form an integral part of LSD, yet require
--   a type name.
class TypeNameOf t where
    typeNameOf :: t -> TypeName

instance TypeNameOf (NamedType t) where
    typeNameOf = ntTypeName
