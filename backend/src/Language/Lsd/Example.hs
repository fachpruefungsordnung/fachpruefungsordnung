-- | Definition of all existing ("example") LSDs.
--
--   This only exists because we cannot yet parse LSD.
module Language.Lsd.Example
    ( availableLSDs
    )
where

import Language.Lsd.AST.Type (NamedType)
import Language.Lsd.AST.Type.DocumentContainer (DocumentContainerType)
import Language.Lsd.Example.Fpo (fpoT)

availableLSDs :: [NamedType DocumentContainerType]
availableLSDs = [fpoT]
