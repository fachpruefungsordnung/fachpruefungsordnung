module Language.Ltml.Tree.Parser.Section
    ( sectionTP
    , sectionBodyTP
    )
where

import Control.Functor.Utils (Pure, pure', sequenceEither)
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
import Language.Ltml.Common (Flagged', ParseError, Parsed)
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
    -> FootnoteTreeParser (Flagged' (Node Section))
sectionTP = nFlaggedTreePF sectionTP'
  where
    sectionTP'
        :: SectionType
        -> InputTree'
        -> FootnoteTreeParser (Node Section)
    sectionTP' t (Leaf x) =
        mkSection t <$> leafFootnoteParser (sectionP t eof) x
    sectionTP' (SectionType kw headingT fmt bodyT) (Tree x children) = do
        wHeading <- lift $ sequenceEither <$> headingTP kw headingT x
        body <- sectionBodyTP bodyT children
        return $ fmap (\heading -> Section fmt heading (Right body)) wHeading

handleLeft :: (Pure f) => (e -> a) -> Either e (f a) -> f a
handleLeft f = either (pure' . f) id

mkSection :: SectionType -> Parsed (Node Section) -> Node Section
mkSection t = handleLeft $ aux t
  where
    aux :: SectionType -> ParseError -> Section
    aux (SectionType _ _ fmt _) e = Section fmt (Left e) (Left e)

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
