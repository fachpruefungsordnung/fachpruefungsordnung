module Language.Ltml.Tree.Parser.Document
    ( documentTP
    , documentTP'
    , documentTXP'
    )
where

import Control.Functor.Utils (Pure, sequenceEither)
import Control.Monad.ConsumableStack
    ( ConsumableStackError
        ( ConsumableStackDepletedEarly
        , ConsumableStackNotFullyConsumed
        )
    , ConsumableStackT
    , pop
    , runConsumableStackT
    )
import Control.Monad.Identity (runIdentity)
import Control.Monad.Trans (lift)
import Data.Text (Text)
import Language.Lsd.AST.Common (Keyword, NavTocHeading)
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
    , DocumentExtro
    , DocumentHeading
    , DocumentIntro
    , DocumentMainBody
    )
import Language.Ltml.AST.Section (SectionBody)
import Language.Ltml.AST.SimpleSection (SimpleSection)
import Language.Ltml.Common (Flagged', NavTocHeaded (NavTocHeaded), Parsed)
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
bodyTP (DocumentBodyType introT mainT extroT) trees =
    runConsumableStackT aux trees >>= either (fail . prettyError) return
  where
    aux :: ConsumableStackT FlaggedInputTree' FootnoteTreeParser DocumentBody
    aux =
        DocumentBody
            <$> traverse (\t -> pop >>= lift . introTP t) introT
            <*> (pop >>= lift . mainTP mainT)
            <*> traverse (\t -> pop >>= lift . extroTP t) extroT

    prettyError ConsumableStackDepletedEarly = "Too few document body children"
    prettyError ConsumableStackNotFullyConsumed =
        "Too many document body children"

introTP
    :: DocumentIntroType
    -> FlaggedInputTree'
    -> FootnoteTreeParser (Flagged' (NavTocHeaded (Parsed DocumentIntro)))
introTP = flaggedTreePF aux
  where
    aux (DocumentIntroType nth t) = introExtroTP' "intro" nth t

extroTP
    :: DocumentExtroType
    -> FlaggedInputTree'
    -> FootnoteTreeParser (Flagged' (NavTocHeaded (Parsed DocumentExtro)))
extroTP = flaggedTreePF aux
  where
    aux (DocumentExtroType nth t) = introExtroTP' "extro" nth t

introExtroTP'
    :: String
    -> NavTocHeading
    -> Sequence (NamedType SimpleSectionType)
    -> InputTree'
    -> FootnoteTreeParser (NavTocHeaded (Parsed [SimpleSection]))
introExtroTP' _ nth t (Leaf x) =
    NavTocHeaded nth
        <$> leafFootnoteParser (simpleSectionSequenceP (fmap unwrapNT t) eof) x
introExtroTP' ename _ _ _ = fail $ "Document " ++ ename ++ " is not leaf"

mainTP
    :: Disjunction DocumentMainBodyType
    -> FlaggedInputTree'
    -> FootnoteTreeParser (Flagged' (NavTocHeaded (Parsed DocumentMainBody)))
mainTP = disjFlaggedTreePF aux
  where
    aux
        :: DocumentMainBodyType
        -> InputTree'
        -> FootnoteTreeParser (NavTocHeaded (Parsed DocumentMainBody))
    aux (DocumentMainBodyType nth t) tree = NavTocHeaded nth <$> aux' tree
      where
        aux' :: InputTree' -> FootnoteTreeParser (Parsed SectionBody)
        aux' (Leaf x) = leafFootnoteParser (sectionBodyP t eof) x
        aux' (Tree (Just _) _) = fail "Document main body has header"
        aux' (Tree Nothing trees) = Right <$> sectionBodyTP t trees
