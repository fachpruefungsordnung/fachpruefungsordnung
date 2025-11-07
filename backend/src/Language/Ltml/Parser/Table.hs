{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.Table
    ( tableP
    )
where

import Data.Text (pack)
import qualified Data.Text as T
import Language.Lsd.AST.SimpleRegex (Star (Star))
import Language.Lsd.AST.Type.Table
    ( CellType (CellType)
    , RowType (RowType)
    , TableType (TableType)
    )
import Language.Ltml.AST.Table
    ( Cell' (Cell', EmptyCell, MergeLeft, MergeUp)
    , Row' (Row')
    , Table' (Table')
    )
import Language.Ltml.AST.Text (TextTree (Word))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Text (textForestP)
import Text.Megaparsec
    ( MonadParsec (eof, hidden, takeWhileP, try)
    , errorBundlePretty
    , manyTill
    , runParser
    , some
    )
import Text.Megaparsec.Char (char, space, string)

cellP :: CellType -> Parser Cell'
cellP (CellType tt) = do
    _ <- char '|'
    space
    -- Take everything until we *see* a table boundary
    chunk <- takeWhileP (Just "cell text") (`notElem` ['|'])
    if
        | T.null chunk -> pure EmptyCell
        | T.strip chunk == "<" -> pure MergeLeft
        | T.strip chunk == "^" -> pure MergeUp
        | otherwise ->
            case runParser (textForestP tt) "" (chunk <> "\n") of
                Left err -> pure (Cell' [Word $ pack $ errorBundlePretty err])
                Right forest -> pure (Cell' forest)

-- parse a row ending with |&
rowP :: RowType -> Parser Row'
rowP (RowType (Star t)) = do
    cells <- nLexeme $ manyTill (cellP t) (try (string "|&"))
    pure (Row' cells)

-- parse an entire table
tableP :: TableType -> Parser Table'
tableP (TableType _ (Star t)) = do
    space
    rows <- nLexeme $ some (rowP t)
    hidden space
    eof
    pure (Table' rows)
