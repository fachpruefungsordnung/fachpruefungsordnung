module Language.Ltml.Tree.Parser.Section
    ( sectionTP
    , sectionBodyTP
    )
where

import Control.Functor.Utils (sequenceEither)
import Control.Monad.Trans.Class (lift)
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
import Language.Ltml.Common (Flagged', Parsed)
import Language.Ltml.Parser.Section (headingP, sectionP)
import Language.Ltml.Tree (FlaggedInputTree', InputTree', Tree (Leaf, Tree))
import Language.Ltml.Tree.Parser
    ( FootnoteTreeParser
    , TreeParser
    , leafFootnoteParser
    , leafParser
    , nFlaggedTreePF
    )
import Text.Megaparsec (eof)

sectionTP
    :: NamedType SectionType
    -> FlaggedInputTree'
    -> FootnoteTreeParser (Flagged' (Parsed (Node Section)))
sectionTP = nFlaggedTreePF sectionTP'
  where
    sectionTP'
        :: SectionType
        -> InputTree'
        -> FootnoteTreeParser (Parsed (Node Section))
    sectionTP' t (Leaf x) = leafFootnoteParser (sectionP t eof) x
    sectionTP' (SectionType kw headingT fmt bodyT) (Tree x children) = do
        wHeading <- lift $ sequenceEither <$> headingTP kw headingT x
        body <- sectionBodyTP bodyT children
        return $ Right $ fmap (\heading -> Section fmt heading body) wHeading

headingTP
    :: Keyword
    -> HeadingType
    -> Maybe Text
    -> TreeParser (Parsed (Node Heading))
headingTP kw t (Just x) = leafParser (headingP kw t) x
headingTP _ _ Nothing = fail "Section lacks heading"

sectionBodyTP
    :: SectionBodyType
    -> [FlaggedInputTree']
    -> FootnoteTreeParser SectionBody
sectionBodyTP (InnerSectionBodyType (Star nt)) trees =
    InnerSectionBody <$> mapM (sectionTP nt) trees
sectionBodyTP _ _ = fail "Invalid section body kind"
