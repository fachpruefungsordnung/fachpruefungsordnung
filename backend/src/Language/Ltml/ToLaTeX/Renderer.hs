{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Renderer
    ( renderLaTeX
    ) where

import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import Language.Ltml.ToLaTeX.LaTeXType (LaTeX (..))

renderLaTeX :: LaTeX -> T.Text
renderLaTeX = LT.toStrict . B.toLazyText . go 0
  where
    go :: Int64 -> LaTeX -> B.Builder
    go _ (Text t) = escape t
    go _ (Raw t) = B.fromText t
    go _ (CommandS name) =
        "\\" <> B.fromText name
    go n (Command name opts args) =
        "\\"
            <> B.fromText name
            <> renderOpts opts
            <> mconcat (map (wrapInBraces . go n) args)
    go n (Environment name opts body) =
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
                        . go (n + 1)
                    )
                    body
                )
            <> B.fromText (T.replicate (fromIntegral n) "\t")
            <> "\\end{"
            <> B.fromText name
            <> "}\n"
    go n (Braced latex) = wrapInBraces (go n latex)
    go n (Sequence xs) = mconcat (map (go n) xs)

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
    escapeChar '_' acc = "\\_" <> acc
    escapeChar '^' acc = "\\^{}" <> acc
    escapeChar '\\' acc = "\\textbackslash{}" <> acc
    escapeChar '{' acc = "\\{" <> acc
    escapeChar '}' acc = "\\}" <> acc
    escapeChar c acc = B.singleton c <> acc
