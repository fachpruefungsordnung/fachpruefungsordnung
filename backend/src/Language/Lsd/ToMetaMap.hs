module Language.Lsd.ToMetaMap
    ( buildMetaMap
    )
where

import Data.Map (Map)
import Language.Lsd.AST.Common (FullTypeName)
import Language.Lsd.AST.Type
    ( NamedType
    , ProperTypeMeta
    , properTypeCollect'
    , properTypeMetaOf
    )
import Language.Lsd.AST.Type.DocumentContainer (DocumentContainerType)

-- | Derive a mapping from full typenames to type metadata, to be used by the
--   frontend.
buildMetaMap
    :: NamedType DocumentContainerType
    -> Map FullTypeName ProperTypeMeta
buildMetaMap = properTypeCollect' properTypeMetaOf
