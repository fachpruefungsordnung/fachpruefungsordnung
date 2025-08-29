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
    , ProperNodeKind (..)
    , RawProperNodeKind (..)
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

-- | A node in the LTML tree is proper iff it corresponds to a node in the
--   input tree ('Language.Ltml.Tree.InputTree').
class ProperNodeKind t where
    kindNameOf :: Proxy t -> KindName
    typeNameOf :: t -> TypeName

-- | An LTML kind @t@ is raw-proper iff @'NamedType' t@ is proper
--   (see 'ProperNodeKind').
class RawProperNodeKind t where
    kindNameOfRaw :: Proxy t -> KindName

instance (RawProperNodeKind t) => ProperNodeKind (NamedType t) where
    kindNameOf _ = kindNameOfRaw (Proxy :: Proxy t)
    typeNameOf = ntTypeName
