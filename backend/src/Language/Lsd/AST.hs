module Language.Lsd.AST
    ( Lsd (..)
    , PreLsd (..)
    , Header (..)
    , PreLsdBody (..)
    )
where

import Data.Map (Map)
import Language.Lsd.AST.Common (TypeName)
import Language.Lsd.AST.Type.Document (DocumentType, PreDocumentType)
import Language.Lsd.AST.Type.Enum (PreEnumType)
import Language.Lsd.AST.Type.Paragraph (PreParagraphType)
import Language.Lsd.AST.Type.Section (PreSectionType)

data Lsd
    = Lsd
        Header
        DocumentType

data PreLsd
    = PreLsd
        Header
        PreLsdBody

data Header = Header

data PreLsdBody = PreLsdBody
    { preDocumentTypes :: Map TypeName PreDocumentType
    , preSectionTypes :: Map TypeName PreSectionType
    , preParagraphTypes :: Map TypeName PreParagraphType
    , preEnumTypes :: Map TypeName PreEnumType
    }
