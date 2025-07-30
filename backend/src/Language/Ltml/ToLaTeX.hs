{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ltml.ToLaTeX 
    (ToLaTeX (..))
    where

import Data.Void (Void, absurd)
import Language.Ltml.AST.Text
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Paragraph (Paragraph (..))
import Language.Ltml.AST.Section (Heading (..), Section (..))
import Language.Ltml.AST.Node (Node (..))
import Language.Ltml.ToLaTeX.Type
import qualified Data.Text.Lazy as LT


class ToLaTeX a where
    toLaTeX :: a -> LaTeX

instance ToLaTeX Void where
    toLaTeX = absurd

-------------------------------- Text -----------------------------------

instance (ToLaTeX enum, 
          ToLaTeX special) 
          => ToLaTeX (TextTree FontStyle enum special) where

    toLaTeX (Word t)              = Text $ LT.fromStrict t
    toLaTeX Space                 = Text $ LT.pack " "
    toLaTeX (Special s)           = toLaTeX s
    toLaTeX (Reference (Label l)) = ref $ LT.fromStrict l
    toLaTeX (Styled style tt)     = applyFontStyle style (map toLaTeX tt)
    toLaTeX (Enum enum)           = toLaTeX enum
    toLaTeX (Footnote tt)         = footnote (map toLaTeX tt)

applyFontStyle :: FontStyle -> [LaTeX] -> LaTeX
applyFontStyle Bold       = bold
applyFontStyle Italics    = italic
applyFontStyle Underlined = underline

instance (ToLaTeX enum, 
          ToLaTeX special) 
          => ToLaTeX (TextTree Void enum special) where

    toLaTeX (Word t)              = Text $ LT.fromStrict t
    toLaTeX Space                 = Text $ LT.pack " "
    toLaTeX (Special s)           = toLaTeX s
    toLaTeX (Reference (Label l)) = ref $ LT.fromStrict l
    toLaTeX (Styled style _)      = absurd style
    toLaTeX (Enum enum)           = toLaTeX enum
    toLaTeX (Footnote tt)         = footnote (map toLaTeX tt)

instance ToLaTeX Enumeration where

    toLaTeX (Enumeration enumItems) = enumerate (map toLaTeX enumItems)

instance ToLaTeX EnumItem where

    toLaTeX (EnumItem tt) = Sequence (map toLaTeX tt)

instance ToLaTeX SentenceStart where

    toLaTeX (SentenceStart mLabel) = maybe mempty toLaTeX mLabel

-------------------------------- Label -----------------------------------

instance ToLaTeX Label where

    toLaTeX (Label t) = label $ LT.fromStrict t

class ToLaTeX a => Labelable a where
    attachLabel :: Maybe Label -> a -> LaTeX


-------------------------------- Paragraph -----------------------------------

instance ToLaTeX Paragraph where

    toLaTeX (Paragraph _ tt) = Sequence (map toLaTeX tt)

instance Labelable Paragraph where
    attachLabel mLabel (Paragraph _ content) =
        Sequence (map toLaTeX content)
        <> maybe mempty toLaTeX mLabel

-------------------------------- Section -----------------------------------
instance ToLaTeX Heading where

    toLaTeX (Heading _ tt) = section (map toLaTeX tt)

instance ToLaTeX Section where

    toLaTeX (Section _ heading eNodes) = either makeSection makeSection eNodes
      where 
        makeSection nodes = section [toLaTeX heading]
                         <> Sequence (map toLaTeX nodes)

instance Labelable Section where
    attachLabel mLabel (Section _ heading nodes) = 
        toLaTeX heading
        <> maybe mempty toLaTeX mLabel
        <> children
      where
        children = case nodes of
            Left subsections -> mconcat (map toLaTeX subsections)
            Right paragraphs -> mconcat (map toLaTeX paragraphs)

-------------------------------- Node -----------------------------------

instance Labelable a => ToLaTeX (Node a) where

    toLaTeX (Node mLabel a) = attachLabel mLabel a


-------------------------------- Document -----------------------------------

-- instance ToLaTeX Document where

--     toLaTeX (Document _ _ (DocumentBody nodes)) = header <> T.intercalate "\n" (map toLaTeX nodes) <> footer
--       where
--         header = "\\documentclass{article}\n"
--               <> "\\usepackage{enumitem}\n"
--               <> "% Define German legal style for enumerations\n"
--               <> "\\setlist[enumerate,1]{label=\\arabic*., left=0pt}\n"
--               <> "\\setlist[enumerate,2]{label=\\alph*), left=1.5em}\n"
--               <> "\\setlist[enumerate,3]{label=\\alph\\alph*), left=3em} \n"
--               <> "\\begin{document}\n"
--         footer = "\n\\end{document}"