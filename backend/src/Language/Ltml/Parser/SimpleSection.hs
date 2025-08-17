module Language.Ltml.Parser.SimpleSection
    ( simpleSectionP
    , simpleSectionSequenceP
    )
where

import Control.Applicative.Utils ((<:>))
import Control.Monad (void)
import Language.Lsd.AST.SimpleRegex (Sequence (Sequence))
import Language.Lsd.AST.Type.SimpleSection
    ( SimpleSectionType (SimpleSectionType)
    )
import Language.Ltml.AST.SimpleSection (SimpleSection (SimpleSection))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Combinators (manyTillSucc)
import Language.Ltml.Parser.Common.Lexeme (nLexeme, nLexeme1)
import Language.Ltml.Parser.Keyword (keywordP)
import Language.Ltml.Parser.SimpleParagraph (simpleParagraphP)

simpleSectionP :: SimpleSectionType -> Parser () -> Parser SimpleSection
simpleSectionP (SimpleSectionType kw fmt childrenT) succStartP = do
    nLexeme1 $ keywordP kw
    SimpleSection fmt
        <$> manyTillSucc (nLexeme $ simpleParagraphP childrenT) succStartP

simpleSectionSequenceP
    :: Sequence SimpleSectionType
    -> Parser ()
    -> Parser [SimpleSection]
simpleSectionSequenceP (Sequence ts') succStartP = aux ts'
  where
    aux [] = return []
    aux [t] = simpleSectionP t succStartP <:> aux []
    aux (t : ts@(t' : _)) = simpleSectionP t (toStartP t') <:> aux ts

toStartP :: SimpleSectionType -> Parser ()
toStartP (SimpleSectionType kw _ _) = void $ keywordP kw
