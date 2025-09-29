{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | module that serves as an edsl to be used to write latex code. only that
--     the code is first transformed into the intermediate structure PreLaTeX.
--     provides all necessary functions and can be extended if needed.
module Language.Ltml.ToLaTeX.PreLaTeXType
    ( PreLaTeX (..)
    {- styling -}
    , text
    , bold
    , italic
    , underline
    , large
    , small
    {- references -}
    , footnote
    , hypertarget
    , hyperlink
    , label
    , footref
    {- commands to structure the text -}
    , medskip
    , hrule
    , linebreak
    , newpage
    {- setup and metadata -}
    , setpdftitle
    , usepackage
    , documentclass
    , fancyhead
    , fancyfoot
    , resetfootnote
    {- environments -}
    , enumerate
    , itemize
    , center
    , flushleft
    , flushright
    , minipage
    , document
    {- other -}
    , setindent
    , setlistdepth
    , renewlist
    , setfontArabic
    , enumStyle
    ) where

import qualified Data.Text as T
import Language.Ltml.AST.Label (Label (Label))

data PreLaTeX
    = IText T.Text
    | -- | raw unescaped PreLaTeX
      IRaw T.Text
    | -- | \command
      ICommandS T.Text
    | -- | \command[opts]{args}
      ICommand T.Text [T.Text] [PreLaTeX]
    | -- | \begin{env}[opts] ... \end{env}
      IEnvironment T.Text [T.Text] [PreLaTeX]
    | -- | used for wrapping in braces
      IBraced PreLaTeX
    | -- | concatenation
      ISequence [PreLaTeX]
    | -- | the reason why we introduced this intermediate data type
      MissingRef Label
    deriving (Show, Eq)

-- | We want to be able to connect PreLaTeX structures and avoid deeply rooted sequences.
--   Here we are using a monoid to be able to concat PreLaTeX structures while flattening sequences.
instance Semigroup PreLaTeX where
    a <> b = sequence' [a, b]
      where
        sequence' :: [PreLaTeX] -> PreLaTeX
        sequence' xs = case flatten xs of
            [x] -> x
            ys -> ISequence ys

        -- \| Flatten nested Sequences as we build them
        flatten :: [PreLaTeX] -> [PreLaTeX]
        flatten = concatMap go
          where
            go (ISequence ys) = flatten ys
            go x = [x]

instance Monoid PreLaTeX where
    mempty = ISequence []

-------------------------------------------------------------------------------
{-                                styling                                   -}

text :: T.Text -> PreLaTeX
text = IText

bold :: PreLaTeX -> PreLaTeX
bold = ICommand "textbf" [] . (: [])

italic :: PreLaTeX -> PreLaTeX
italic = ICommand "emph" [] . (: [])

underline :: PreLaTeX -> PreLaTeX
underline = ICommand "underline" [] . (: [])

large :: PreLaTeX -> PreLaTeX
large content = IBraced $ ICommand "large" [] [content]

small :: PreLaTeX -> PreLaTeX
small content = IBraced $ ICommand "small" [] [content]

-------------------------------------------------------------------------------
{-                             referencing                                -}

footnote :: PreLaTeX -> PreLaTeX
footnote = ICommand "footnote" [] . (: [])

hypertarget :: Label -> PreLaTeX -> PreLaTeX
hypertarget (Label l) latex = ICommand "hypertarget" [] [IRaw l, latex]

hyperlink :: Label -> PreLaTeX -> PreLaTeX
hyperlink (Label l) latex = ICommand "hyperlink" [] [IRaw l, latex]

label :: T.Text -> PreLaTeX
label l = ICommand "label" [] [IRaw l]

footref :: T.Text -> PreLaTeX
footref r = ICommand "footref" [] [IRaw r]

-------------------------------------------------------------------------------
{-                             setup and metadata                             -}

usepackage :: [T.Text] -> T.Text -> PreLaTeX
usepackage opts package = ICommand "usepackage" opts [IText package]

documentclass :: [T.Text] -> T.Text -> PreLaTeX
documentclass opts name = ICommand "documentclass" opts [IText name]

fancyhead :: [T.Text] -> PreLaTeX -> PreLaTeX
fancyhead opts content = ICommand "fancyhead" opts [content]

fancyfoot :: [T.Text] -> PreLaTeX -> PreLaTeX
fancyfoot opts content = ICommand "fancyfoot" opts [content]

resetfootnote :: PreLaTeX
resetfootnote = ICommand "setcounter" [] [IText "footnote", IText "0"]

setpdftitle :: T.Text -> PreLaTeX
setpdftitle title = ICommand "hypersetup" [] [IText $ "pdftitle={" <> title <> "}"]

-------------------------------------------------------------------------------
{-                              text structure                              -}

linebreak :: PreLaTeX
linebreak = IRaw "\\\\"

newpage :: PreLaTeX
newpage = ICommandS "newpage"

medskip :: PreLaTeX
medskip = IText "\n" <> ICommandS "medskip" <> IRaw "\n"

hrule :: PreLaTeX
hrule = ICommandS "hrule"

-------------------------------------------------------------------------------
{-                              environments                                 -}

enumerate :: [T.Text] -> [PreLaTeX] -> PreLaTeX
enumerate opts items = IEnvironment "enumerate" opts (map (\i -> ICommand "item" [] [i]) items)

itemize :: [PreLaTeX] -> PreLaTeX
itemize items = IEnvironment "itemize" [] (map (\i -> ICommand "item" [] [i]) items)

center :: [PreLaTeX] -> PreLaTeX
center = IEnvironment "center" []

flushleft :: [PreLaTeX] -> PreLaTeX
flushleft = IEnvironment "flushleft" []

flushright :: [PreLaTeX] -> PreLaTeX
flushright = IEnvironment "flushright" []

minipage :: [T.Text] -> [PreLaTeX] -> PreLaTeX
minipage = IEnvironment "minipage"

document :: PreLaTeX -> PreLaTeX
document content = IEnvironment "document" [] [content]

-------------------------------------------------------------------------------
{-                              other                                        -}

setindent :: PreLaTeX
setindent = IRaw "\\setlength{\\parindent}{0pt}"

setlistdepth :: PreLaTeX
setlistdepth = IRaw "\\setlistdepth{9}"

renewlist :: PreLaTeX
renewlist = IRaw "\\renewlist{enumerate}{enumerate}{9}"

setfontArabic :: PreLaTeX
setfontArabic =
    ISequence
        [ usepackage [] "helvet"
        , IRaw "\\renewcommand{\\familydefault}{\\sfdefault}"
        ]

enumStyle :: PreLaTeX
enumStyle =
    IRaw "\\setlist[enumerate,1]{label=\\arabic*., left=0pt}"
        <> IRaw "\\setlist[enumerate,2]{label=\\alph*., left=0.5em}"
        <> IRaw "\\setlist[enumerate,3]{label=\\alph*\\alph*., left=1em}"
        <> IRaw "\\setlist[enumerate,4]{label=-, left=1.5em}"
        <> IRaw "\\setlist{nosep}"
