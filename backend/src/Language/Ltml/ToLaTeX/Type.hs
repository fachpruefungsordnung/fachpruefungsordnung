{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Language.Ltml.ToLaTeX.Type (
    LaTeX (..),
    text, 
    bold,
    italic,
    underline,
    footnote,
    label, 
    ref, 
    paragraph,
    large,
    linebreak,
    enumerate,
    itemize,
    center
) where

import qualified Data.Text.Lazy as LT
import Language.Ltml.AST.Label (Label)

data LaTeX
  = Text LT.Text
  | Raw LT.Text                                -- raw unescaped LaTeX
  | MissingRef Label
  | Command LT.Text [LT.Text] [LaTeX]          -- \command[opts]{args}
  | Environment LT.Text [LT.Text] [LaTeX]      -- \begin{env}[opts] ... \end{env}
  | Sequence [LaTeX]                           -- concatenation
  deriving (Show, Eq)

-- | We want to be able to connect LaTeX structures and avoid deeply rooted sequences.
--   Here we are using a monoid to be able to concat LaTeX structures while flattening sequences. 
instance Semigroup LaTeX where

    (<>) :: LaTeX -> LaTeX -> LaTeX
    a <> b = sequence' [a, b]
      where
        sequence' :: [LaTeX] -> LaTeX
        sequence' xs = case flatten xs of
                         [x] -> x
                         ys  -> Sequence ys

        -- Flatten nested Sequences as we build them
        flatten :: [LaTeX] -> [LaTeX]
        flatten = concatMap go
            where
                go (Sequence ys) = flatten ys
                go x             = [x]

instance Monoid LaTeX where
    mempty = Sequence []



-------------------------------------------------------------------------------
{-                                commands                                   -}

text :: LT.Text -> LaTeX
text = Text

bold :: [LaTeX] -> LaTeX
bold = Command "textbf" []

italic :: [LaTeX] -> LaTeX
italic = Command "emph" []

underline :: [LaTeX] -> LaTeX
underline = Command "underline" []

footnote :: [LaTeX] -> LaTeX
footnote = Command "footnote" []

-------------------------------------------------------------------------------
{-                             text structure                                -}

label :: LT.LazyText -> LaTeX
label l = Command "label" [] [Text l]

ref :: LT.LazyText -> LaTeX
ref r = Command "ref" [] [Text r]

paragraph :: LaTeX -> LaTeX -> LaTeX
paragraph identifier content = Environment "tabularx" [] [Raw "{\\linewidth}{@{}lX@{}}", Sequence [identifier, Raw " & ", content]]

large :: LaTeX -> LaTeX
large content = Raw "{\\Large " <> content <> Raw "}"

linebreak :: LaTeX
linebreak = Raw "\\\\"

-------------------------------------------------------------------------------
{-                              environments                                 -}

enumerate :: [LaTeX] -> LaTeX
enumerate items = Environment "enumerate" [] (map (\i -> Command "item" [] [i]) items)

itemize :: [LaTeX] -> LaTeX
itemize items = Environment "itemize" [] (map (\i -> Command "item" [] [i]) items)

center :: [LaTeX] -> LaTeX
center = Environment "center" []


