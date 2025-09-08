module Language.Ltml.ToLaTeX.LaTeXType
    ( LaTeX (..)
    ) where

import qualified Data.Text as T

data LaTeX
    = Text T.Text
    | Raw T.Text -- raw unescaped LaTeX
    | CommandS T.Text -- \command
    | Command T.Text [T.Text] [LaTeX] -- \command[opts]{args}
    | Environment T.Text [T.Text] [LaTeX] -- \begin{env}[opts] ... \end{env}
    | Braced LaTeX -- used for wrapping in braces
    | Sequence [LaTeX] -- concatenation
    deriving (Show)
