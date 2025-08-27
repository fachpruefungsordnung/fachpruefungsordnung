module Language.Ltml.Tree.Parser.Document
    ( documentTP
    , documentTP'
    , documentTXP'
    )
where

import Control.Functor.Utils (sequenceF)
import Control.Monad.Identity (runIdentity)
import Data.Text (Text)
import Language.Lsd.AST.Common (Keyword)
import Language.Lsd.AST.SimpleRegex (Disjunction (Disjunction), Sequence)
import Language.Lsd.AST.Type
    ( NamedType
    , unwrapNT
    )
import Language.Lsd.AST.Type.Document
    ( DocumentBodyType (DocumentBodyType)
    , DocumentHeadingType
    , DocumentMainBodyType (DocumentMainBodyType)
    , DocumentType (DocumentType)
    )
import Language.Lsd.AST.Type.Section (SectionBodyType)
import Language.Lsd.AST.Type.SimpleSection (SimpleSectionType)
import Language.Ltml.AST.Document
    ( Document (Document)
    , DocumentBody (DocumentBody)
    , DocumentHeading
    )
import Language.Ltml.AST.Section (SectionBody)
import Language.Ltml.AST.SimpleSection (SimpleSection)
import Language.Ltml.Common (Flagged (Flagged))
import Language.Ltml.Parser.Document (documentHeadingP)
import Language.Ltml.Parser.Footnote (unwrapFootnoteParser)
import Language.Ltml.Parser.Section (sectionBodyP)
import Language.Ltml.Parser.SimpleSection (simpleSectionSequenceP)
import Language.Ltml.Parser.Text (HangingTextP)
import Language.Ltml.Tree (Tree (Leaf, Tree), TypedTree)
import Language.Ltml.Tree.Parser
    ( TreeParser
    , disjStaticTypedTreePF
    , leafParser
    , nTypedTreePF
    , staticTypedTreePF
    , treeError
    )
import Language.Ltml.Tree.Parser.Footnote
    ( FootnoteTreeParser
    , leafFootnoteParser
    )
import Language.Ltml.Tree.Parser.Section (sectionBodyTP)
import Text.Megaparsec (eof)

documentTP
    :: NamedType DocumentType
    -> TypedTree
    -> TreeParser Document
documentTP nt tTree = runIdentity <$> documentTP' nt tTree

documentTP'
    :: (HangingTextP f)
    => NamedType DocumentType
    -> TypedTree
    -> TreeParser (f Document)
documentTP' = nTypedTreePF documentTXP'

documentTXP'
    :: (HangingTextP f)
    => DocumentType
    -> Tree
    -> TreeParser (f Document)
documentTXP' _ (Leaf _) =
    treeError "Parsing textual documents not yet implemented" -- TODO
documentTXP'
    (DocumentType kw fmt headingT bodyT (Disjunction fnTs))
    (Tree x children) = do
        wHheading <- sequenceF <$> headingTP kw headingT x
        (body, fnMap) <-
            unwrapFootnoteParser (map unwrapNT fnTs) $
                bodyTP bodyT children
        return $ fmap (\heading -> Document fmt heading body fnMap) wHheading

headingTP
    :: (HangingTextP f)
    => Keyword
    -> DocumentHeadingType
    -> Maybe (Flagged Text)
    -> TreeParser (Flagged (f DocumentHeading))
headingTP kw t (Just x) = leafParser (documentHeadingP kw t) x
headingTP _ _ Nothing = treeError "Document lacks heading"

bodyTP :: DocumentBodyType -> [TypedTree] -> FootnoteTreeParser DocumentBody
bodyTP (DocumentBodyType introT mainT extroT) [intro, main, extro] =
    DocumentBody
        <$> introExtroTP introT intro
        <*> mainTP mainT main
        <*> introExtroTP extroT extro
bodyTP _ _ = treeError "Invalid number of document body children"

introExtroTP
    :: Sequence (NamedType SimpleSectionType)
    -> TypedTree
    -> FootnoteTreeParser (Flagged [SimpleSection])
introExtroTP = staticTypedTreePF introExtroTP'
  where
    introExtroTP' t (Leaf x) =
        leafFootnoteParser (simpleSectionSequenceP (fmap unwrapNT t) eof) x
    introExtroTP' _ _ = treeError "Document intro/extro is not leaf"

mainTP
    :: Disjunction DocumentMainBodyType
    -> TypedTree
    -> FootnoteTreeParser (Flagged SectionBody)
mainTP = disjStaticTypedTreePF (aux . \(DocumentMainBodyType t) -> t)
  where
    aux :: SectionBodyType -> Tree -> FootnoteTreeParser (Flagged SectionBody)
    aux t (Leaf x) = leafFootnoteParser (sectionBodyP t eof) x
    aux _ (Tree (Just _) _) = treeError "Document main body has header"
    aux t (Tree Nothing trees) = Flagged False <$> sectionBodyTP t trees
