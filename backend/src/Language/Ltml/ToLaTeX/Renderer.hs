{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Renderer
    ( renderLaTeX
    ) where

import Data.Int (Int64)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import Language.Ltml.ToLaTeX.LaTeXType (LaTeX (..))

renderLaTeX :: LaTeX -> LT.Text
renderLaTeX = B.toLazyText . go 0
  where
    go :: Int64 -> LaTeX -> B.Builder
    go _ (Text t) = escape t
    go _ (Raw t) = B.fromLazyText t
    go _ (CommandS name) =
        "\\"
            <> B.fromLazyText name
    go n (Command name opts args) =
        "\\"
            <> B.fromLazyText name
            <> renderOpts opts
            <> mconcat (map (wrapInBraces . go n) args)
    go n (Environment name opts body) =
        "\n"
            <> B.fromLazyText (LT.replicate n "\t")
            <> "\\begin{"
            <> B.fromLazyText name
            <> "}"
            <> renderOpts opts
            <> "\n"
            <> mconcat
                ( map
                    ((B.fromLazyText (LT.replicate (n + 1) "\t") <>) . (<> "\n") . go (n + 1))
                    body
                )
            <> B.fromLazyText (LT.replicate n "\t")
            <> "\\end{"
            <> B.fromLazyText name
            <> "}\n"
    go n (Braced latex) = wrapInBraces (go n latex)
    go n (Sequence xs) = mconcat (map (go n) xs)

    renderOpts :: [LT.Text] -> B.Builder
    renderOpts [] = mempty
    renderOpts os = "[" <> B.fromLazyText (LT.intercalate "," os) <> "]"

    wrapInBraces :: B.Builder -> B.Builder
    wrapInBraces b = "{" <> b <> "}"

    escape :: LT.Text -> B.Builder
    escape = LT.foldr escapeChar mempty

    escapeChar :: Char -> B.Builder -> B.Builder
    escapeChar '#' acc = "\\#" <> acc
    escapeChar '$' acc = "\\$" <> acc
    escapeChar '%' acc = "\\%" <> acc
    escapeChar '&' acc = "\\&" <> acc
    escapeChar '~' acc = "\\~{}" <> acc
    escapeChar '_' acc = "\\_" <> acc
    escapeChar '^' acc = "\\^{}" <> acc
    escapeChar '\\' acc = "\\textbackslash{}" <> acc
    escapeChar '{' acc = "\\{" <> acc
    escapeChar '}' acc = "\\}" <> acc
    escapeChar c acc = B.singleton c <> acc
