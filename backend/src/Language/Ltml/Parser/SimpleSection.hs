module Language.Ltml.Parser.SimpleSection
    ( simpleSectionP
    , simpleSectionSequenceP
    )
where

import Control.Applicative.Utils ((<:>))
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Language.Lsd.AST.SimpleRegex (Sequence (Sequence), Star (Star))
import Language.Lsd.AST.Type (unwrapNT)
import Language.Lsd.AST.Type.SimpleSection
    ( SimpleSectionType (SimpleSectionType)
    )
import Language.Ltml.AST.SimpleSection (SimpleSection (SimpleSection))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme1)
import Language.Ltml.Parser.Footnote (FootnoteParser)
import Language.Ltml.Parser.Footnote.Combinators (manyWithFootnotesTillSucc)
import Language.Ltml.Parser.Keyword (keywordP)
import Language.Ltml.Parser.SimpleParagraph (simpleParagraphP)

simpleSectionP
    :: SimpleSectionType
    -> Parser ()
    -> FootnoteParser SimpleSection
simpleSectionP (SimpleSectionType kw fmt (Star childrenT)) succStartP = do
    lift $ nLexeme1 $ keywordP kw
    SimpleSection fmt
        <$> manyWithFootnotesTillSucc
            (simpleParagraphP $ unwrapNT childrenT)
            succStartP

simpleSectionSequenceP
    :: Sequence SimpleSectionType
    -> Parser ()
    -> FootnoteParser [SimpleSection]
simpleSectionSequenceP (Sequence ts') succStartP = aux ts'
  where
    aux [] = lift $ [] <$ succStartP
    aux [t] = simpleSectionP t succStartP <:> aux []
    aux (t : ts@(t' : _)) = simpleSectionP t (toStartP t') <:> aux ts

toStartP :: SimpleSectionType -> Parser ()
toStartP (SimpleSectionType kw _ _) = void $ keywordP kw
