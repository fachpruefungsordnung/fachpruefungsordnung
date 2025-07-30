{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Renderer (
    renderLaTeX
) where

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import Language.Ltml.ToLaTeX.Type
import Data.Int (Int64)

renderLaTeX :: LaTeX -> LT.Text
renderLaTeX = B.toLazyText . go 0
  where
    go :: Int64 -> LaTeX -> B.Builder
    go _ (Text t)      = escape t
    go _ (Raw t)       = B.fromLazyText t
    go n (Command name opts args) =
      "\\" <> B.fromLazyText name
           <> renderOpts opts
           <> "{" <> mconcat (map (go n) args) <> "}"
    go n (Environment name opts body) =
      "\n" <> B.fromLazyText (LT.replicate n "\t") <> "\\begin{" <> B.fromLazyText name <> "}\n"
      <> renderOpts opts
      <> mconcat (map ((B.fromLazyText (LT.replicate (n+1) "\t") <>) . (<> "\n") . go (n+1)) body)
      <> B.fromLazyText (LT.replicate n "\t") <> "\\end{" <> B.fromLazyText name <> "}\n"
    go n (Sequence xs)      = mconcat (map (go n) xs)

    renderOpts :: [LT.Text] -> B.Builder
    renderOpts [] = mempty
    renderOpts os = "[" <> B.fromLazyText (LT.intercalate "," os) <> "]"

    escape :: LT.Text -> B.Builder
    escape = LT.foldr escapeChar mempty

    escapeChar :: Char -> B.Builder -> B.Builder
    escapeChar '#'  acc = "\\#" <> acc
    escapeChar '$'  acc = "\\$" <> acc
    escapeChar '%'  acc = "\\%" <> acc
    escapeChar '&'  acc = "\\&" <> acc
    escapeChar '~'  acc = "\\~{}" <> acc
    escapeChar '_'  acc = "\\_" <> acc
    escapeChar '^'  acc = "\\^{}" <> acc
    escapeChar '\\' acc = "\\textbackslash{}" <> acc
    escapeChar '{'  acc = "\\{" <> acc
    escapeChar '}'  acc = "\\}" <> acc
    escapeChar c    acc = B.singleton c <> acc
