{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Language.Ltml.Parser.Table
    ( tableP
    , padTable
    )
where

import qualified Data.Text as T
import Language.Ltml.AST.Table (Cell_ (Cell_, EmptyCell, MergeLeft, MergeUp), Row_ (Row_), Table_ (Table_))
import Language.Ltml.Parser (Parser)
import Text.Megaparsec
    ( MonadParsec(eof, takeWhileP, try, hidden),
      runParser,
      errorBundlePretty,
      manyTill,
      some )
import Text.Megaparsec.Char ( char, space, string )
import Language.Lsd.AST.Type.Table (CellType (CellType), RowType (RowType), TableType (TableType))
import Language.Ltml.Parser.Text (textForestP)
import Language.Lsd.AST.SimpleRegex (Star(Star))
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.AST.Text (TextTree(Word))
import Data.Text (pack)


cellP :: CellType -> Parser Cell_
cellP (CellType tt) = do
  _ <- char '|'
  space
  -- Take everything until we *see* a table boundary
  chunk <- takeWhileP (Just "cell text") (`notElem` ['|'])
  if
    | T.null chunk        -> pure EmptyCell
    | T.strip chunk == "<"        -> pure MergeLeft
    | T.strip chunk == "^"        -> pure MergeUp
    | otherwise         ->
        case runParser (textForestP tt) "" (chunk <> "\n") of
          Left err     -> pure (Cell_ [Word $ pack $ errorBundlePretty err])
          Right forest -> pure (Cell_ forest)


-- parse a row ending with |&
rowP :: RowType -> Parser Row_
rowP (RowType (Star t))= do
    cells <- nLexeme $ manyTill (cellP t) (try (string "|&"))
    pure (Row_ cells)

-- parse an entire table
tableP :: TableType -> Parser Table_
tableP (TableType _ (Star t)) = do
    space
    rows <- nLexeme $ some (rowP t)
    hidden space
    eof
    pure (Table_ rows)

-- pad rows to equal length
padTable :: Table_ -> Table_
padTable (Table_ rows) =
    let maxLen = maximum (map rowLen rows)
     in Table_ (map (padRow maxLen) rows)
  where
    rowLen (Row_ cs) = length cs
    padRow n (Row_ cs) =
        Row_ (cs ++ replicate (n - length cs) EmptyCell)
