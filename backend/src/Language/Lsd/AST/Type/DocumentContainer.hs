{-# LANGUAGE OverloadedStrings #-}

module Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerFormat (..)
    , DocumentContainerType (..)
    , HeaderFooterFormat (..)
    , HeaderFooterItemFormat (..)
    , HeaderFooterFormatAtom (..)
    )
where

import Data.Typography (FontSize, FontStyle)
import Language.Lsd.AST.Format (FormatString, MainHeadingFormat)
import Language.Lsd.AST.SimpleRegex (Sequence (Sequence))
import Language.Lsd.AST.Type
    ( ChildrenOrder (SequenceOrder)
    , HasEditableHeader (HasEditableHeader)
    , NamedType
    , NavHeadingGeneration (NavHeadingStatic)
    , RawProperNodeKind (..)
    , TreeSyntax (TreeSyntax)
    )
import Language.Lsd.AST.Type.AppendixSection (AppendixSectionType)
import Language.Lsd.AST.Type.Document (DocumentType)

data DocumentContainerFormat
    = -- | format
      DocumentContainerFormat
        HeaderFooterFormat
        -- ^ header format
        HeaderFooterFormat
        -- ^ footer format
        MainHeadingFormat
        -- ^ format of the main document's heading
    deriving (Show)

data DocumentContainerType
    = DocumentContainerType
        DocumentContainerFormat
        (NamedType DocumentType)
        (Sequence (NamedType AppendixSectionType))

instance RawProperNodeKind DocumentContainerType where
    kindNameOfRaw _ = "document-container"

    treeSyntaxMapRaw f (DocumentContainerType _ mainDocT (Sequence appSecsT)) =
        TreeSyntax (HasEditableHeader True) $
            SequenceOrder $
                pure <$> (f mainDocT : map f appSecsT)

    navHeadingGenerationOfRaw _ = NavHeadingStatic "(header)"

-- | The format of a printed header/footer.
data HeaderFooterFormat
    = -- | format
      HeaderFooterFormat
        [HeaderFooterItemFormat]
        -- ^ left
        [HeaderFooterItemFormat]
        -- ^ middle
        [HeaderFooterItemFormat]
        -- ^ right
    deriving (Show)

data HeaderFooterItemFormat
    = HeaderFooterItemFormat
        FontSize
        [FontStyle]
        (FormatString HeaderFooterFormatAtom)
    deriving (Show)

data HeaderFooterFormatAtom
    = HeaderFooterSuperTitleAtom
    | HeaderFooterTitleAtom
    | HeaderFooterDateAtom
    | HeaderFooterCurPageNumAtom
    | HeaderFooterLastPageNumAtom
    deriving (Show)
