{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.Table
    ( tableP
    , padTable
    )
where

import Control.Monad (void)
import qualified Data.Text as T
import Language.Ltml.AST.Table (Cell (Cell), Row (Row), Table (Table))
import Language.Ltml.Parser (Parser)
import Text.Megaparsec
import Text.Megaparsec.Char

-- tableP :: TableType -> Parser Table
-- tableP (TableType kw) = undefined --Table <$ nLexeme1 (keywordP kw)

textCellP :: Parser T.Text
textCellP = T.strip <$> takeWhileP (Just "cell text") (`notElem` ['|', '&', '\n'])

-- parse a single cell
cellP :: Parser Cell
cellP = do
    void $ char '|'
    space
    content <- textCellP
    space
    pure (Cell content)

-- parse a row ending with |&
rowP :: Parser Row
rowP = do
    cells <- manyTill cellP (try (string "|&"))
    space
    pure (Row cells)

-- parse an entire table
tableP :: Parser Table
tableP = do
    space
    rows <- some rowP
    eof
    pure (Table rows)

-- pad rows to equal length
padTable :: Table -> Table
padTable (Table rows) =
    let maxLen = maximum (map rowLen rows)
     in Table (map (padRow maxLen) rows)
  where
    rowLen (Row cs) = length cs
    padRow n (Row cs) =
        Row (cs ++ replicate (n - length cs) (Cell T.empty))
