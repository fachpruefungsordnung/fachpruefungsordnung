module Language.Lsd.AST.Format
    ( FormatString (..)
    , FormatAtom (..)
    , IdentifierFormat
    , EnumStyle (..)
    , HeadingFormat
    , HeadingPlaceholderAtom (..)
    , KeyFormat
    , KeyPlaceholderAtom (..)
    , TocKeyFormat (..)
    , EnumItemKeyFormat (..)
    , ParagraphKeyFormat (..)
    )
where

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

type HeadingFormat = FormatString HeadingPlaceholderAtom

data HeadingPlaceholderAtom
    = IdentifierPlaceholder
    | HeadingTextPlaceholder
    deriving (Show)

type KeyFormat = FormatString KeyPlaceholderAtom

data KeyPlaceholderAtom = KeyIdentifierPlaceholder
    deriving (Show, Eq)

newtype TocKeyFormat = TocKeyFormat KeyFormat
    deriving (Show)

newtype EnumItemKeyFormat = EnumItemKeyFormat KeyFormat
    deriving (Show, Eq)

newtype ParagraphKeyFormat = ParagraphKeyFormat KeyFormat
    deriving (Show)
