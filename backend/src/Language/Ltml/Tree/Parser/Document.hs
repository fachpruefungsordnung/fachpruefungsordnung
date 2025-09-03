module Language.Ltml.Tree.Parser.Document
    ( documentTP
    , documentTP'
    , documentTXP'
    )
where

import Control.Functor.Utils (Pure, sequenceEither)
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
import Language.Ltml.Common (Flagged', Parsed)
import Language.Ltml.Parser.Document (documentHeadingP)
import Language.Ltml.Parser.Footnote (runFootnoteWriterT)
import Language.Ltml.Parser.Section (sectionBodyP)
import Language.Ltml.Parser.SimpleSection (simpleSectionSequenceP)
import Language.Ltml.Parser.Text (HangingTextP)
import Language.Ltml.Tree (FlaggedInputTree', InputTree', Tree (Leaf, Tree))
import Language.Ltml.Tree.Parser
    ( FootnoteTreeParser
    , TreeParser
    , disjFlaggedTreePF
    , flaggedTreePF
    , leafFootnoteParser
    , leafParser
    , nFlaggedTreePF
    , treeError
    )
import Language.Ltml.Tree.Parser.Section (sectionBodyTP)
import Text.Megaparsec (eof)

documentTP
    :: NamedType DocumentType
    -> FlaggedInputTree'
    -> TreeParser (Flagged' Document)
documentTP nt tTree = fmap runIdentity <$> documentTP' nt tTree

documentTP'
    :: (Pure f, HangingTextP f)
    => NamedType DocumentType
    -> FlaggedInputTree'
    -> TreeParser (Flagged' (f Document))
documentTP' = nFlaggedTreePF documentTXP'

documentTXP'
    :: (Pure f, HangingTextP f)
    => DocumentType
    -> InputTree'
    -> TreeParser (f Document)
documentTXP' _ (Leaf _) =
    treeError "Parsing textual documents not yet implemented" -- TODO
documentTXP'
    (DocumentType kw fmt headingT bodyT (Disjunction fnTs))
    (Tree x children) = do
        wHheading <- sequenceEither <$> headingTP kw headingT x
        (body, fnMap) <-
            runFootnoteWriterT (bodyTP bodyT children) (map unwrapNT fnTs)
        return $ fmap (\heading -> Document fmt heading body fnMap) wHheading

headingTP
    :: (HangingTextP f)
    => Keyword
    -> DocumentHeadingType
    -> Maybe Text
    -> TreeParser (Parsed (f DocumentHeading))
headingTP kw t (Just x) = leafParser (documentHeadingP kw t) x
headingTP _ _ Nothing = treeError "Document lacks heading"

bodyTP
    :: DocumentBodyType
    -> [FlaggedInputTree']
    -> FootnoteTreeParser DocumentBody
bodyTP (DocumentBodyType introT mainT extroT) [intro, main, extro] =
    DocumentBody
        <$> introExtroTP introT intro
        <*> mainTP mainT main
        <*> introExtroTP extroT extro
bodyTP _ _ = treeError "Invalid number of document body children"

introExtroTP
    :: Sequence (NamedType SimpleSectionType)
    -> FlaggedInputTree'
    -> FootnoteTreeParser (Flagged' (Parsed [SimpleSection]))
introExtroTP = flaggedTreePF introExtroTP'
  where
    introExtroTP' t (Leaf x) =
        leafFootnoteParser (simpleSectionSequenceP (fmap unwrapNT t) eof) x
    introExtroTP' _ _ = treeError "Document intro/extro is not leaf"

mainTP
    :: Disjunction DocumentMainBodyType
    -> FlaggedInputTree'
    -> FootnoteTreeParser (Flagged' (Parsed SectionBody))
mainTP = disjFlaggedTreePF (aux . \(DocumentMainBodyType t) -> t)
  where
    aux
        :: SectionBodyType
        -> InputTree'
        -> FootnoteTreeParser (Parsed SectionBody)
    aux t (Leaf x) = leafFootnoteParser (sectionBodyP t eof) x
    aux _ (Tree (Just _) _) = treeError "Document main body has header"
    aux t (Tree Nothing trees) = Right <$> sectionBodyTP t trees
