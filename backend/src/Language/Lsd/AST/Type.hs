{-# LANGUAGE RankNTypes #-}
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
    , ProperTypeMeta (..)
    , TreeSyntax (..)
    , HasEditableHeader (..)
    , ChildrenOrder (..)
    , ProperNodeKind (..)
    , RawProperNodeKind (..)
    , fullTypeNameOf
    , properTypeMetaOf
    , properTypeCollect
    , properTypeCollect'
    )
where

import Control.Monad.CollectionState
    ( CollectionState
    , collect
    , execCollectionState
    )
import Data.Map (Map)
import Data.Proxy (Proxy (Proxy))
import Language.Lsd.AST.Common
    ( DisplayTypeName
    , FullTypeName
    , KindName
    , TypeName
    )
import Language.Lsd.AST.SimpleRegex (Disjunction (Disjunction))

data NamedType t
    = NamedType
    { ntTypeName :: TypeName
    , ntDisplayName :: DisplayTypeName
    , unwrapNT :: t
    }

-- | Metadata on a proper (see 'ProperNodeKind') LTML type.
--   To be communicated to and used by the frontend only.  Not used for
--   parsing; some information is duplicated in the parser.
data ProperTypeMeta
    = ProperTypeMeta
        DisplayTypeName
        (TreeSyntax FullTypeName)
    deriving (Show)

-- | Syntax of an input tree ('Language.Ltml.Tree.FlaggedInputTree').
--   This information is duplicated in the input tree parser (and, arguably,
--   in the LTML and LSD ASTs, the latter of which it is derived from).
data TreeSyntax a
    = LeafSyntax
    | TreeSyntax
        HasEditableHeader
        (ChildrenOrder a)
    deriving (Show)

newtype HasEditableHeader = HasEditableHeader Bool
    deriving (Show)

-- | Information on permitted proper (see 'ProperNodeKind') children of proper
--   nodes and their order.
data ChildrenOrder a
    = SequenceOrder [Disjunction a]
    | StarOrder (Disjunction a)
    deriving (Show)

-- | A node in the LTML tree is proper iff it corresponds to a node in the
--   input tree ('Language.Ltml.Tree.InputTree').
class ProperNodeKind t where
    kindNameOf :: Proxy t -> KindName
    typeNameOf :: t -> TypeName
    displayTypeNameOf :: t -> DisplayTypeName
    treeSyntaxMap
        :: (forall t'. (ProperNodeKind t') => t' -> a)
        -> t
        -> TreeSyntax a

    -- | Whether a kind has a heading that can be displayed in a TOC.
    --   Used only for the main navigation TOC / heading-tree in the frontend;
    --   other TOCs generally only include part of the full TOC.
    --   Not to be confused with whether a node has an editable header.
    kindHasTocHeading :: Proxy t -> Bool

-- | An LTML kind @t@ is raw-proper iff @'NamedType' t@ is proper
--   (see 'ProperNodeKind').
class RawProperNodeKind t where
    kindNameOfRaw :: Proxy t -> KindName
    treeSyntaxMapRaw
        :: (forall t'. (ProperNodeKind t') => t' -> a)
        -> t
        -> TreeSyntax a
    kindHasTocHeadingRaw :: Proxy t -> Bool

instance (RawProperNodeKind t) => ProperNodeKind (NamedType t) where
    kindNameOf _ = kindNameOfRaw (Proxy :: Proxy t)
    typeNameOf = ntTypeName
    displayTypeNameOf = ntDisplayName
    treeSyntaxMap f = treeSyntaxMapRaw f . unwrapNT
    kindHasTocHeading _ = kindHasTocHeadingRaw (Proxy :: Proxy t)

fullTypeNameOf :: forall t. (ProperNodeKind t) => t -> FullTypeName
fullTypeNameOf t = (kindNameOf (Proxy :: Proxy t), typeNameOf t)

properTypeMetaOf :: (ProperNodeKind t) => t -> ProperTypeMeta
properTypeMetaOf t =
    ProperTypeMeta
        (displayTypeNameOf t)
        (treeSyntaxMap fullTypeNameOf t)

properChildrenTypeMap
    :: (ProperNodeKind t)
    => (forall t'. (ProperNodeKind t') => t' -> a)
    -> t
    -> [a]
properChildrenTypeMap f t =
    case treeSyntaxMap f t of
        LeafSyntax -> []
        TreeSyntax _ (SequenceOrder ts') -> concatMap aux ts'
        TreeSyntax _ (StarOrder t') -> aux t'
  where
    aux (Disjunction ts') = ts'

-- | Collect information on proper (see 'ProperNodeKind') tree types
--   recursively.
--   This should generally be applied to roots of the type tree; that is, to
--   'Language.Lsd.AST.Type.DocumentContainerType's.
properTypeCollect
    :: (ProperNodeKind t, Ord k)
    => (forall t'. (ProperNodeKind t') => t' -> (k, v))
    -> t
    -> Map k v
properTypeCollect f = execCollectionState . aux f
  where
    aux
        :: (ProperNodeKind t, Ord k)
        => (forall t'. (ProperNodeKind t') => t' -> (k, v))
        -> t
        -> CollectionState k v ()
    aux f' t =
        let (k, v) = f' t
         in collect k $ v <$ sequence (properChildrenTypeMap (aux f') t)

-- | Specialized variant of `properTypeCollect`.
properTypeCollect'
    :: (ProperNodeKind t)
    => (forall t'. (ProperNodeKind t') => t' -> v)
    -> t
    -> Map FullTypeName v
properTypeCollect' f = properTypeCollect (\t' -> (fullTypeNameOf t', f t'))
