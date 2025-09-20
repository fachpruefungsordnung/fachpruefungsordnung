{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Renderer
    ( renderLaTeX
    , renderLaTeXPretty
    ) where

import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import Language.Ltml.ToLaTeX.LaTeXType (LaTeX (..))

renderLaTeX :: LaTeX -> T.Text
renderLaTeX = LT.toStrict . B.toLazyText . build
  where
    build :: LaTeX -> B.Builder
    build (Text t) = escape t
    build (Raw t) = B.fromText t
    build (CommandS name) =
        "\\" <> B.fromText name
    build (Command name opts args) =
        "\\"
            <> B.fromText name
            <> renderOpts opts
            <> mconcat (map (wrapInBraces . build) args)
    build (Environment name opts body) =
        "\\begin{"
            <> B.fromText name
            <> "}"
            <> renderOpts opts
            <> mconcat (map build body)
            <> "\\end{"
            <> B.fromText name
            <> "}"
    build (Braced latex) = wrapInBraces (build latex)
    build (Sequence xs) = mconcat (map build xs)

renderLaTeXPretty :: LaTeX -> T.Text
renderLaTeXPretty = LT.toStrict . B.toLazyText . build 0
  where
    build :: Int64 -> LaTeX -> B.Builder
    build _ (Text t) = escape t
    build _ (Raw t) = B.fromText t
    build _ (CommandS name) =
        "\\" <> B.fromText name
    build n (Command name opts args) =
        "\\"
            <> B.fromText name
            <> renderOpts opts
            <> mconcat (map (wrapInBraces . build n) args)
    build n (Environment name opts body) =
        "\n"
            <> B.fromText (T.replicate (fromIntegral n) "\t")
            <> "\\begin{"
            <> B.fromText name
            <> "}"
            <> renderOpts opts
            <> "\n"
            <> mconcat
                ( map
                    ( (B.fromText (T.replicate (fromIntegral (n + 1)) "\t") <>)
                        . (<> "\n")
                        . build (n + 1)
                    )
                    body
                )
            <> B.fromText (T.replicate (fromIntegral n) "\t")
            <> "\\end{"
            <> B.fromText name
            <> "}\n"
    build n (Braced latex) = wrapInBraces (build n latex)
    build n (Sequence xs) = mconcat (map (build n) xs)

renderOpts :: [T.Text] -> B.Builder
renderOpts [] = mempty
renderOpts os = "[" <> B.fromText (T.intercalate "," os) <> "]"

wrapInBraces :: B.Builder -> B.Builder
wrapInBraces b = "{" <> b <> "}"

escape :: T.Text -> B.Builder
escape = T.foldr escapeChar mempty

escapeChar :: Char -> B.Builder -> B.Builder
escapeChar '#' acc = "\\#" <> acc
escapeChar '$' acc = "\\$" <> acc
escapeChar '%' acc = "\\%" <> acc
escapeChar '&' acc = "\\&" <> acc
escapeChar '~' acc = "\\~{}" <> acc
escapeChar '_' acc = "-" <> acc
escapeChar '^' acc = "\\^{}" <> acc
escapeChar '\\' acc = "\\textbackslash{}" <> acc
escapeChar '{' acc = "\\{" <> acc
escapeChar '}' acc = "\\}" <> acc
escapeChar c acc = B.singleton c <> acc
