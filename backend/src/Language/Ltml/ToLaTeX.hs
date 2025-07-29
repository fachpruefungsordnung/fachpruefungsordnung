{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Ltml.ToLaTeX 
    (ToLaTeX (..))
    where

import Data.Text (Text, pack)
import Data.Void (Void, absurd)
import qualified Data.Text as T (concat, intercalate)
import Language.Ltml.AST.Text
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Paragraph (Paragraph (..))
import Language.Ltml.AST.Section (Heading (..), Section (..))
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.AST.Document (Document (..), DocumentBody (..))


class ToLaTeX a where
    toLaTeX :: a -> Text

instance ToLaTeX Void where
    toLaTeX = absurd

-------------------------------- Text -----------------------------------
instance (ToLaTeX style, 
          ToLaTeX enum, 
          ToLaTeX special) 
          => ToLaTeX (TextTree style enum special) where

    toLaTeX (Word text)           = text
    toLaTeX Space                 = pack " "
    toLaTeX (Special s)           = toLaTeX s
    toLaTeX (Reference (Label l)) = "\\ref{" <> l <> "}"
    toLaTeX (Styled style tt)     = "\\" <> toLaTeX style <> "{" <> T.concat (map toLaTeX tt) <> "}"        
    toLaTeX (Enum enum)           = toLaTeX enum
    toLaTeX (Footnote tt)         = "\\footnote{" <> T.concat (map toLaTeX tt) <> "}"

instance ToLaTeX FontStyle where
    toLaTeX Bold       = "textbf"
    toLaTeX Italics    = "textit"
    toLaTeX Underlined = "underline"

instance ToLaTeX Enumeration where

    toLaTeX (Enumeration enumItems) = "\n\\begin{enumerate}\n" 
                                      <> T.intercalate "\n" (map toLaTeX enumItems) 
                                      <> "\n\\end{enumerate}\n"

instance ToLaTeX EnumItem where

    toLaTeX (EnumItem tt) = "\t\\item " <> T.concat (map toLaTeX tt)

instance ToLaTeX SentenceStart where

    toLaTeX (SentenceStart mLabel) = maybe "" toLaTeX mLabel

-------------------------------- Label -----------------------------------

instance ToLaTeX Label where

    toLaTeX (Label t) = "\\label{" <> t <> "}"

-------------------------------- Paragraph -----------------------------------

instance ToLaTeX Paragraph where

    toLaTeX (Paragraph _ tt) = T.concat (map toLaTeX tt)

-------------------------------- Section -----------------------------------
instance ToLaTeX Heading where

    toLaTeX (Heading _ tt) = T.concat (map toLaTeX tt)

instance ToLaTeX Section where

    toLaTeX (Section _ heading (Left nodes)) = "\\section{" <> toLaTeX heading <> "}\n" 
                                            <> T.intercalate "\n" (map toLaTeX nodes) 
    toLaTeX (Section _ heading (Right nodes)) = "\\section{" <> toLaTeX heading <> "}\n" 
                                            <> T.intercalate "\n" (map toLaTeX nodes) 

-------------------------------- Node -----------------------------------

instance ToLaTeX a => ToLaTeX (Node a) where

    toLaTeX (Node mLabel a) = maybe "" toLaTeX mLabel <> " " <> toLaTeX a

-------------------------------- Document -----------------------------------

instance ToLaTeX Document where

    toLaTeX (Document _ _ (DocumentBody nodes)) = header <> T.intercalate "\n" (map toLaTeX nodes) <> footer
      where
        header = "\\documentclass{article}\n"
              <> "\\usepackage{enumitem}\n"
              <> "% Define German legal style for enumerations\n"
              <> "\\setlist[enumerate,1]{label=\\arabic*., left=0pt}\n"
              <> "\\setlist[enumerate,2]{label=\\alph*), left=1.5em}\n"
              <> "\\setlist[enumerate,3]{label=\\alph\\alph*), left=3em} \n"
              <> "\\begin{document}\n"
        footer = "\n\\end{document}"