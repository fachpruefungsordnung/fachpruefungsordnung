module Language.Ltml.ToLaTeX.LaTeXType (
      LaTeX (..)
    ) where

import qualified Data.Text.Lazy as LT

data LaTeX
    = Text LT.Text
    | Raw LT.Text -- raw unescaped LaTeX
    | CommandS LT.Text -- \command
    | Command LT.Text [LT.Text] [LaTeX] -- \command[opts]{args}
    | Environment LT.Text [LT.Text] [LaTeX] -- \begin{env}[opts] ... \end{env}
    | Braced LaTeX -- used for wrapping in braces
    | Sequence [LaTeX] -- concatenation
    deriving (Show)
