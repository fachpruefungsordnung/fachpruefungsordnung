{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Type
    ( LaTeX (..)
    , text
    , bold
    , italic
    , underline
    , footnote
    , hypertarget
    , hyperlink
    , label
    , footref
    , paragraph
    , large
    , small
    , medskip
    , resetFootnoteCTR
    , usepackage
    , documentclass
    , linebreak
    , enumerate
    , itemize
    , center
    , flushleft
    , flushright
    , minipage
    , document
    , setindent
    , setfontArabic
    , enumStyle
    ) where

import qualified Data.Text.Lazy as LT
import Language.Ltml.AST.Label (Label (Label))

data LaTeX
    = Text LT.Text
    | Raw LT.Text -- raw unescaped LaTeX
    | MissingRef Label
    | Command LT.Text [LT.Text] [LaTeX] -- \command[opts]{args}
    | Environment LT.Text [LT.Text] [LaTeX] -- \begin{env}[opts] ... \end{env}
    | Sequence [LaTeX] -- concatenation
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
            ys -> Sequence ys

        -- Flatten nested Sequences as we build them
        flatten :: [LaTeX] -> [LaTeX]
        flatten = concatMap go
          where
            go (Sequence ys) = flatten ys
            go x = [x]

instance Monoid LaTeX where
    mempty = Sequence []

-------------------------------------------------------------------------------
{-                                commands                                   -}

text :: LT.Text -> LaTeX
text = Text

bold :: LaTeX -> LaTeX
bold = Command "textbf" [] . (: [])

italic :: LaTeX -> LaTeX
italic = Command "emph" [] . (: [])

underline :: LaTeX -> LaTeX
underline = Command "underline" [] . (: [])

footnote :: LaTeX -> LaTeX
footnote = Command "footnote" [] . (: [])

hypertarget :: Label -> LaTeX -> LaTeX
hypertarget (Label l) latex = Command "hypertarget" [] [Text (LT.fromStrict l), latex]

hyperlink :: Label -> LaTeX -> LaTeX
hyperlink (Label l) latex = Command "hyperlink" [] [Text (LT.fromStrict l), latex]

usepackage :: [LT.Text] -> LT.Text -> LaTeX
usepackage opts package = Command "usepackage" opts [Text package]

documentclass :: [LT.Text] -> LT.Text -> LaTeX
documentclass opts name = Command "documentclass" opts [Text name]

parbox :: [LT.Text] -> [LaTeX] -> LaTeX
parbox = Command "parbox"

-------------------------------------------------------------------------------
{-                             text structure                                -}

label :: LT.Text -> LaTeX
label l = Raw $ "\\label{" <> l <> "}"

footref :: LT.Text -> LaTeX
footref r = Raw $ "\\footref{" <> r <> "}"

large :: LaTeX -> LaTeX
large content = Raw "{\\large " <> content <> Raw "}"

small :: LaTeX -> LaTeX
small content = Raw "{\\small " <> content <> Raw "}"

linebreak :: LaTeX
linebreak = Raw "\\\\"

medskip :: LaTeX
medskip = Raw "\n\\medskip\n"

hfill :: LaTeX
hfill = Raw "\\hfill"

resetFootnoteCTR :: LaTeX
resetFootnoteCTR = Raw "\\setcounter{footnote}{0}"

-------------------------------------------------------------------------------
{-                              environments                                 -}

enumerate :: [LT.Text] -> [LaTeX] -> LaTeX
enumerate opts items = Environment "enumerate" opts (map (\i -> Command "item" [] [i]) items)

itemize :: [LaTeX] -> LaTeX
itemize items = Environment "itemize" [] (map (\i -> Command "item" [] [i]) items)

center :: [LaTeX] -> LaTeX
center = Environment "center" []

flushleft :: [LaTeX] -> LaTeX
flushleft = Environment "flushleft" []

flushright :: [LaTeX] -> LaTeX
flushright = Environment "flushright" []

minipage :: [LT.Text] -> [LaTeX] -> LaTeX
minipage = Environment "minipage"

paragraph :: LaTeX -> LaTeX -> LaTeX
paragraph identifier content =
    Sequence
        [ parbox ["t"] [Raw "2em", identifier]
        , hfill
        , parbox ["t"] [Raw "\\dimexpr\\linewidth-2em-0.5em\\relax", content]
        , Raw "\n"
        ]

document :: LaTeX -> LaTeX
document content = Environment "document" [] [content]

-------------------------------------------------------------------------------
{-                              other                                        -}

setindent :: LaTeX
setindent = Raw "\\setlength{\\parindent}{0pt}"

setfontArabic :: LaTeX
setfontArabic =
    Sequence
        [ usepackage [] "helvet"
        , Raw "\\renewcommand{\\familydefault}{\\sfdefault}"
        ]

enumStyle :: LaTeX
enumStyle =
    Raw "\\setlist[enumerate,1]{label=\\arabic*., left=0pt}"
        <> Raw "\\setlist[enumerate,2]{label=\\alph*., left=0.5em}"
        <> Raw "\\setlist[enumerate,3]{label=\\alph*\\alph*., left=1em}"
        <> Raw "\\setlist[enumerate,4]{label=-, left=1.5em}"
        <> Raw "\\setlist{nosep}"
