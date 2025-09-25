-- | Module that provides a LaTeX data type
module Language.Ltml.ToLaTeX.LaTeXType
    ( LaTeX (..)
    ) where

import qualified Data.Text as T

data LaTeX
    = Text T.Text
    | -- | raw unescaped LaTeX
      Raw T.Text
    | -- | \command
      CommandS T.Text
    | -- | \command[opts]{args}
      Command T.Text [T.Text] [LaTeX]
    | -- | \begin{env}[opts] ... \end{env}
      Environment T.Text [T.Text] [LaTeX]
    | -- | used for wrapping in braces
      Braced LaTeX
    | -- | concatenation
      Sequence [LaTeX]
    deriving (Show)
