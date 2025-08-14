{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Format
    ( formatHeading
    , formatKey
    , staticDocumentFormat
    , getIdentifier
    , getEnumStyle
    ) where

import Data.Char (chr)
import qualified Data.Text.Lazy as LT
import Language.Lsd.AST.Format
import Language.Lsd.AST.Type.Enum
    ( EnumFormat (EnumFormat)
    , EnumItemFormat (EnumItemFormat)
    )
import Language.Ltml.ToLaTeX.Type
import Prelude hiding (id)

formatHeading :: HeadingFormat -> LaTeX -> LaTeX -> LaTeX
formatHeading (FormatString []) _ _ = mempty
formatHeading (FormatString (StringAtom s : rest)) i latex =
    Sequence (map replace s) <> formatHeading (FormatString rest) i latex
  where
    replace '\n' = linebreak
    replace c = Text (LT.pack [c])
formatHeading (FormatString (PlaceholderAtom a : rest)) i latex =
    case a of
        HeadingTextPlaceholder -> latex <> formatHeading (FormatString rest) i latex
        IdentifierPlaceholder -> i <> formatHeading (FormatString rest) i latex

formatKey :: KeyFormat -> LaTeX -> LaTeX
formatKey (FormatString []) _ = mempty
formatKey (FormatString (StringAtom s : rest)) n =
    Text (LT.pack s) <> formatKey (FormatString rest) n
formatKey (FormatString (PlaceholderAtom KeyIdentifierPlaceholder : rest)) n =
    n <> formatKey (FormatString rest) n

staticDocumentFormat :: LaTeX
staticDocumentFormat =
    Sequence
        [ documentclass [] "article"
        , setfontArabic
        , usepackage
            [ "letterpaper"
            , "top=2cm"
            , "bottom=2cm"
            , "left=3cm"
            , "right=3cm"
            , "marginparwidth=1.75cm"
            ]
            "geometry"
        , usepackage
            [ "colorlinks=true"
            , "allcolors=red"
            ]
            "hyperref"
        , usepackage [] "enumitem"
        , usepackage [] "tabularx"
        , usepackage ["T1"] "fontenc"
        , enumStyle
        , setindent
        ]

getIdentifier :: IdentifierFormat -> Int -> LT.Text
getIdentifier (FormatString []) _ = mempty
getIdentifier (FormatString (StringAtom s : rest)) i =
    LT.pack s <> getIdentifier (FormatString rest) i
getIdentifier (FormatString (PlaceholderAtom a : rest)) i =
    case a of
        Arabic -> LT.pack (show i) <> getIdentifier (FormatString rest) i
        AlphabeticLower -> LT.pack [chr ((i - 1) `mod` 27 + 97)] <> getIdentifier (FormatString rest) i
        AlphabeticUpper -> LT.pack [chr ((i - 1) `mod` 27 + 65)] <> getIdentifier (FormatString rest) i

getEnumStyle :: EnumFormat -> LT.Text
getEnumStyle (EnumFormat (EnumItemFormat ident key)) = "label=" <> buildKey (getEnumIdentifier' ident) key
  where
    buildKey :: LT.Text -> EnumItemKeyFormat -> LT.Text
    buildKey _ (EnumItemKeyFormat (FormatString [])) = mempty
    buildKey id (EnumItemKeyFormat (FormatString (StringAtom s : rest))) =
        LT.pack s <> buildKey id (EnumItemKeyFormat (FormatString rest))
    buildKey
        id
        ( EnumItemKeyFormat
                (FormatString (PlaceholderAtom KeyIdentifierPlaceholder : rest))
            ) =
            id <> buildKey id (EnumItemKeyFormat (FormatString rest))

    getEnumIdentifier' :: IdentifierFormat -> LT.Text
    getEnumIdentifier' (FormatString []) = mempty
    getEnumIdentifier' (FormatString (StringAtom s : rest)) =
        LT.pack s <> getEnumIdentifier' (FormatString rest)
    getEnumIdentifier' (FormatString (PlaceholderAtom a : rest)) =
        case a of
            Arabic -> "\\arabic*" <> getEnumIdentifier' (FormatString rest)
            AlphabeticLower -> "\\alph*" <> getEnumIdentifier' (FormatString rest)
            AlphabeticUpper -> "\\Alph*" <> getEnumIdentifier' (FormatString rest)
