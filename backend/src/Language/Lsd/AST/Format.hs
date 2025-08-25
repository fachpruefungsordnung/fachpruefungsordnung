{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Language.Lsd.AST.Format
    ( FormatString (..)
    , FormatAtom (..)
    , IdentifierFormat
    , EnumStyle (..)
    , HeadingFormat (..)
    , HeadingPlaceholderAtom (..)
    , MainHeadingFormat
    , InnerHeadingFormat
    , KeyFormat
    , KeyPlaceholderAtom (..)
    , TocKeyFormat (..)
    , EnumItemKeyFormat (..)
    , ParagraphKeyFormat (..)
    )
where

import Data.Typography (Typography)

newtype FormatString a = FormatString [FormatAtom a]
    deriving (Show, Eq)

data FormatAtom a
    = StringAtom String
    | PlaceholderAtom a
    deriving (Show, Eq)

type IdentifierFormat = FormatString EnumStyle

data EnumStyle
    = Arabic
    | AlphabeticLower
    | AlphabeticUpper
    deriving (Show, Eq)

-- | The format of a heading.  The boolean type parameter determines whether
--   an identifier (placeholder) is permitted in the heading (format).
data HeadingFormat (permitIdentifier :: Bool)
    = HeadingFormat
        Typography
        (FormatString (HeadingPlaceholderAtom permitIdentifier))
    deriving (Show)

data HeadingPlaceholderAtom (permitIdentifier :: Bool) where
    IdentifierPlaceholder :: HeadingPlaceholderAtom 'True
    HeadingTextPlaceholder :: HeadingPlaceholderAtom permitIdentifier

deriving instance Show (HeadingPlaceholderAtom a)

-- | Heading format without identifier placeholders.
type MainHeadingFormat = HeadingFormat 'False

-- | Heading format with identifier placeholders.
type InnerHeadingFormat = HeadingFormat 'True

type KeyFormat = FormatString KeyPlaceholderAtom

data KeyPlaceholderAtom = KeyIdentifierPlaceholder
    deriving (Show, Eq)

newtype TocKeyFormat = TocKeyFormat KeyFormat
    deriving (Show)

newtype EnumItemKeyFormat = EnumItemKeyFormat KeyFormat
    deriving (Show, Eq)

newtype ParagraphKeyFormat = ParagraphKeyFormat KeyFormat
    deriving (Show)
