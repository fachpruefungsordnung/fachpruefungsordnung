module Language.Ltml.Tree.Parser.Section
    ( sectionTP
    , sectionBodyTP
    )
where

import Control.Functor.Utils (sequenceF)
import Data.Text (Text)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Star (Star))
import Language.Lsd.AST.Type (NamedType)
import Language.Lsd.AST.Type.Section
    ( HeadingType
    , SectionBodyType (InnerSectionBodyType)
    , SectionType (SectionType)
    )
import Language.Ltml.AST.Node (Node)
import Language.Ltml.AST.Section
    ( Heading
    , Section (Section)
    , SectionBody (InnerSectionBody)
    )
import Language.Ltml.Common (Flagged (Flagged))
import Language.Ltml.Parser.Section (headingP, sectionP)
import Language.Ltml.Tree (Tree (Leaf, Tree), TypedTree)
import Language.Ltml.Tree.Parser
    ( TreeParser
    , leafParser
    , nTypedTreePF
    , treeError
    , wrapTreeParser
    )
import Language.Ltml.Tree.Parser.Footnote
    ( FootnoteTreeParser
    , leafFootnoteParser
    )
import Text.Megaparsec (eof)

sectionTP
    :: NamedType SectionType
    -> TypedTree
    -> FootnoteTreeParser (Flagged (Node Section))
sectionTP = nTypedTreePF sectionTP'
  where
    sectionTP'
        :: SectionType
        -> Tree
        -> FootnoteTreeParser (Flagged (Node Section))
    sectionTP' t (Leaf x) = leafFootnoteParser (sectionP t eof) x
    sectionTP' (SectionType kw headingT fmt bodyT) (Tree x children) = do
        wHeading <- wrapTreeParser $ sequenceF <$> headingTP kw headingT x
        body <- sectionBodyTP bodyT children
        return $
            Flagged False $
                fmap (\heading -> Section fmt heading body) wHeading

headingTP
    :: Keyword
    -> HeadingType
    -> Maybe (Flagged Text)
    -> TreeParser (Flagged (Node Heading))
headingTP kw t (Just x) = leafParser (headingP kw t) x
headingTP _ _ Nothing = treeError "Section lacks heading"

sectionBodyTP
    :: SectionBodyType
    -> [TypedTree]
    -> FootnoteTreeParser SectionBody
sectionBodyTP (InnerSectionBodyType (Star nt)) trees =
    InnerSectionBody <$> mapM (sectionTP nt) trees
sectionBodyTP _ _ = treeError "Invalid section body kind"
