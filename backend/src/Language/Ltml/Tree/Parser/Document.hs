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
    , DocumentExtroType (DocumentExtroType)
    , DocumentHeadingType
    , DocumentIntroType (DocumentIntroType)
    , DocumentMainBodyType (DocumentMainBodyType)
    , DocumentType (DocumentType)
    )
import Language.Lsd.AST.Type.SimpleSection (SimpleSectionType)
import Language.Ltml.AST.Document
    ( Document (Document)
    , DocumentBody (DocumentBody)
    , DocumentExtro (DocumentExtro)
    , DocumentHeading
    , DocumentIntro (DocumentIntro)
    , DocumentMainBody (DocumentMainBody)
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
    fail "Parsing textual documents not yet implemented" -- TODO
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
headingTP _ _ Nothing = fail "Document lacks heading"

bodyTP
    :: DocumentBodyType
    -> [FlaggedInputTree']
    -> FootnoteTreeParser DocumentBody
bodyTP (DocumentBodyType introT mainT extroT) [intro, main, extro] =
    DocumentBody
        <$> introTP introT intro
        <*> mainTP mainT main
        <*> extroTP extroT extro
bodyTP _ _ = fail "Invalid number of document body children"

introTP
    :: DocumentIntroType
    -> FlaggedInputTree'
    -> FootnoteTreeParser (Flagged' (Parsed DocumentIntro))
introTP = flaggedTreePF aux
  where
    aux (DocumentIntroType fmt t) tree =
        fmap (DocumentIntro fmt) <$> introExtroTP' "intro" t tree

extroTP
    :: DocumentExtroType
    -> FlaggedInputTree'
    -> FootnoteTreeParser (Flagged' (Parsed DocumentExtro))
extroTP = flaggedTreePF aux
  where
    aux (DocumentExtroType fmt t) tree =
        fmap (DocumentExtro fmt) <$> introExtroTP' "extro" t tree

introExtroTP'
    :: String
    -> Sequence (NamedType SimpleSectionType)
    -> InputTree'
    -> FootnoteTreeParser (Parsed [SimpleSection])
introExtroTP' _ t (Leaf x) =
    leafFootnoteParser (simpleSectionSequenceP (fmap unwrapNT t) eof) x
introExtroTP' ename _ _ = fail $ "Document " ++ ename ++ " is not leaf"

mainTP
    :: Disjunction DocumentMainBodyType
    -> FlaggedInputTree'
    -> FootnoteTreeParser (Flagged' (Parsed DocumentMainBody))
mainTP = disjFlaggedTreePF aux
  where
    aux
        :: DocumentMainBodyType
        -> InputTree'
        -> FootnoteTreeParser (Parsed DocumentMainBody)
    aux (DocumentMainBodyType fmt t) tree =
        fmap (DocumentMainBody fmt) <$> aux' tree
      where
        aux' :: InputTree' -> FootnoteTreeParser (Parsed SectionBody)
        aux' (Leaf x) = leafFootnoteParser (sectionBodyP t eof) x
        aux' (Tree (Just _) _) = fail "Document main body has header"
        aux' (Tree Nothing trees) = Right <$> sectionBodyTP t trees
