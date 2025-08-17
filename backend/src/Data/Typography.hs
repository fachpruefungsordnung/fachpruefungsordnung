module Data.Typography
    ( TextAlignment (..)
    , FontSize (..)
    , FontStyle (..)
    )
where

data TextAlignment
    = LeftAligned
    | Centered
    | RightAligned
    deriving (Show)

data FontSize
    = SmallFontSize
    | MediumFontSize
    | LargeFontSize
    deriving (Show)

data FontStyle
    = Bold
    | Italics
    | Underlined
    deriving (Show)
