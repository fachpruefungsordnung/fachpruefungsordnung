{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Parser.Table
    ( tableP
    )
where

import Data.Array (Array, array, bounds, (!), (//))
import Data.Text (pack)
import qualified Data.Text as T
import Language.Lsd.AST.Common (Keyword (Keyword))
import Language.Lsd.AST.SimpleRegex (Disjunction (Disjunction), Star (Star))
import Language.Lsd.AST.Type.Table
    ( CellFormat
    , CellType (CellType)
    , DefaultCellType (DefaultCellType)
    , RowType (RowType)
    , TableType (TableType)
    )
import Language.Ltml.AST.Table
    ( Cell (..)
    , Row (..)
    , Table (..)
    )
import Language.Ltml.AST.Text (TableTextTree, TextTree (Word))
import Language.Ltml.Parser (Parser)
import Language.Ltml.Parser.Common.Lexeme (nLexeme)
import Language.Ltml.Parser.Text (textForestP)
import Text.Megaparsec
    ( MonadParsec (hidden, takeWhileP, try)
    , choice
    , errorBundlePretty
    , manyTill
    , runParser
    , some
    , (<|>), optional
    )
import Text.Megaparsec.Char (space, string)
import Text.Megaparsec.Char.Lexer (decimal)

cellP :: Keyword -> CellType -> Parser Cell'
cellP (Keyword tkw) (CellType (Keyword ckw) fmt tt) = do
    _ <- string (tkw <> ckw)
    space
    -- Take everything until we *see* a table boundary
    chunk <- takeWhileP (Just "cell text") (`notElem` ['|'])
    if
        | T.null chunk -> pure $ Cell' fmt (Cell'' [])
        | T.strip chunk == "<" -> pure $ Cell' fmt MergeLeft
        | T.strip chunk == "^" -> pure $ Cell' fmt MergeUp
        | otherwise ->
            case runParser (textForestP tt) "" (chunk <> "\n") of
                Left err -> pure $ Cell' fmt (Cell'' [Word $ pack $ errorBundlePretty err])
                Right forest -> pure $ Cell' fmt (Cell'' forest)

-- parse a row ending with |&
rowP :: Keyword -> DefaultCellType -> RowType -> Parser Row'
rowP k@(Keyword tkw) (DefaultCellType defaultCellT) (RowType (Keyword rkw) (Star (Disjunction t))) = do
    cells <-
        nLexeme $
            manyTill
                ( choice
                    (map (cellP k) t)
                    <|> cellP k defaultCellT
                )
                (try (string (tkw <> rkw)))
    -- _ <- string (tkw <> rkw)
    pure (Row' cells)

-- parse an entire table
tableP :: TableType -> Parser Table
tableP (TableType kw defaultCellT (Star t)) = do
    space
    mProps <- optional $ nLexeme $ some propP
    nRows <- nLexeme $ some (rowP kw defaultCellT t)
    hidden space
    -- _ <- char '&'
    pure (mergeCells mProps defaultCellT $ Table' nRows)
  where
    propP :: Parser Int
    propP = do 
        _ <- string "|="
        space
        n <- decimal
        space
        return n


-- the rawly parsed representation of a table
newtype Table' = Table' {unTable' :: [Row']}

newtype Row' = Row' {unRow' :: [Cell']}

data Cell' = Cell' CellFormat Cell''

data Cell'' = Cell'' [TableTextTree] | MergeLeft | MergeUp
    deriving (Show)

instance Eq Cell'' where
    (Cell'' _) == (Cell'' _) = True
    MergeLeft == MergeLeft = True
    MergeUp == MergeUp = True
    _ == _ = False

-- everything to merge cells and compute spans

type Matrix a = Array (Int, Int) a
type Visited = Array (Int, Int) VisitedCell
type Visited = Array (Int, Int) VisitedCell
type Position = (Int, Int)
data VisitedCell = Unvisited | Visited | InSpan Int deriving (Show)

instance Eq VisitedCell where
    Unvisited == Unvisited = True
    Visited == Visited = True
    (InSpan {}) == (InSpan {}) = True
    _ == _ = False
data VisitedCell = Unvisited | Visited | InSpan Int deriving (Show)

instance Eq VisitedCell where
    Unvisited == Unvisited = True
    Visited == Visited = True
    (InSpan {}) == (InSpan {}) = True
    _ == _ = False

mergeCells :: Maybe [Int] -> DefaultCellType -> Table' -> Table
mergeCells mProps (DefaultCellType defaultCellT) table =
    let matrix' =
            array
                ((0, 0), (nRows, mCols))
                [((row, col), emptyEntry) | row <- [0 .. nRows], col <- [0 .. mCols]]
        visited0 =
            array
                ((0, 0), (nRows, mCols))
                [((row, col), Unvisited) | row <- [0 .. nRows], col <- [0 .. mCols]]
     in Table mProps $ arrayToTable $ snd $ process (0, 0) (visited0, matrix')
  where
    table' = padTable defaultCellT table
    rawMatrix = tableToArray table'
    ((0, 0), (nRows, mCols)) = bounds rawMatrix
    emptyEntry = HSpannedCell
    emptyEntry = HSpannedCell

    unpackCell' (Cell' _ c) = c

    maxWidth :: Position -> Int
    maxWidth (row, col) =
        (1 +) $
            length $
                takeWhile
                    (\col' -> col' <= mCols && unpackCell' (rawMatrix ! (row, col')) == MergeLeft)
                    [col + 1 .. mCols]

    maxHeight :: Position -> Int -> Int
    maxHeight (row, col) w =
        (1 +) $
            length $
                takeWhile
                    ( \row' ->
                        row' <= nRows
                            && unpackCell' (rawMatrix ! (row', col)) == MergeUp
                            && all
                                ( \col' ->
                                    let val = unpackCell' (rawMatrix ! (row', col'))
                                     in val == MergeLeft || val == MergeUp
                                )
                                [col + 1 .. col + w - 1]
                    )
                    [row + 1 .. nRows]

    -- updateVisited :: Position -> (Visited, Matrix Cell) -> (Visited, Matrix Cell)
    -- updateVisited p0 (visited, mat) = (\p -> p == p0 || visited p, mat)
    -- updateVisited :: Position -> (Visited, Matrix Cell) -> (Visited, Matrix Cell)
    -- updateVisited p0 (visited, mat) = (\p -> p == p0 || visited p, mat)

    updateMatrix
        :: Position -> Cell -> (Visited, Matrix Cell) -> (Visited, Matrix Cell)
    updateMatrix pos val (visited, mat) = (visited, mat // [(pos, val)])

    process :: Position -> (Visited, Matrix Cell) -> (Visited, Matrix Cell)
    process (row, col) s@(visited, matrix)
    process (row, col) s@(visited, matrix)
        | row > nRows = s
        | col > mCols = process (row + 1, 0) s
        | visited ! (row, col) == Visited = process (row, col + 1) s
        | visited ! (row, col) == InSpan 0 = -- this is safe, regard the def of Eq VisitedCell. (even though maybe a little dirty...)
            let InSpan w = visited ! (row, col)
                matrix' = matrix // [((row, col), VSpannedCell w)]
             in process (row, col + 1) (visited, matrix')
        | otherwise =
            let createCell fmt content =
                    let w = maxWidth (row, col)
                        h = maxHeight (row, col) w
                        s' =
                            foldl
                                ( \(v, m) (x, y) ->
                                    ( if y == col && row < x
                                        then v // [((x, y), InSpan w)]
                                        else v // [((x, y), Visited)]
                                    , m
                                    )
                                )
                                s
                                [ (row', col')
                                | col' <- [col .. col + w - 1]
                                , row' <- [row .. row + h - 1]
                                ]
                     in updateMatrix (row, col) (Cell fmt content w h) s'
                res = case rawMatrix ! (row, col) of
                    Cell' fmt MergeLeft -> createCell fmt [Word "<"]
                    Cell' fmt MergeUp -> createCell fmt [Word "^"]
                    Cell' fmt (Cell'' content) -> createCell fmt content
             in process (row, col + 1) res

    -- Convert array back to list of lists (optional)
    arrayToTable :: Matrix Cell -> [Row]
    arrayToTable arr =
        let ((i0, j0), (in_, jn)) = bounds arr
        in [Row [arr ! (row, col) | col <- [j0 .. jn]] | row <- [i0 .. in_]]

-- Convert a list of lists to an array
tableToArray :: Table' -> Matrix Cell'
tableToArray t =
    let nRows = length (unTable' t) - 1
        mCols = if nRows < 0 then 0 else length (unRow' (head (unTable' t))) - 1
     in array
            ((0, 0), (nRows, mCols))
            [((row, col), getCell row col) | row <- [0 .. nRows], col <- [0 .. mCols]]
  where
    rows = unTable' t
    getCell row col = unRow' (rows !! row) !! col


-- pad nRows to equal length
padTable :: CellType -> Table' -> Table'
padTable (CellType _ fmt _) (Table' rows) =
    let maxLen = maximum (map rowLen rows)
     in Table' (map (padRow maxLen) rows)
  where
    rowLen (Row' cs) = length cs
    padRow row (Row' cs) =
        Row' (cs ++ replicate (row - length cs) (Cell' fmt (Cell'' [])))

{-
Example table Ltml representation:
\|* <*PHF-phil-BA1>         |* <*Philosophische Fach- und Vermittlungskompetenzen>                                    |<|<|<|<|<|<|&
\| <*Semesterlage>          | <*Dauer>                 |<|<| <*Status> | <*Zugangsvoraussetzung> | <*LP/Workload>               |<|&
\| 1. und 2. Semester       | 2 Semester               |<|<| Pflicht   | -                       | 9 LP / 270 Stunden           |<|&
\| <*Lehrveranstaltunge(n)> | <*Lehrform> | <*SWS> | <*LP> | <*Status> | <*Prüfungsleistungen>   | <*Bewertungsart> | <*Wichtung> |&
\| Einführung in die
  Philosophie              | Vorlesung   | 2      | 2     | Pflicht   | -                       | -                | -           |&
\| Logik, Argumentation,
  Sprache                  | *Seminar    | 2      | 4     | Pflicht   | Take-home-Klausur (ca.
                                                                        5 Seiten) oder Klausur
                                                                        (3 Std.)                | unbenotet        | -           |&
\| Einführung in das
  Verfassen
  wissenschaftlicher
  Texte im Fach
  Philosophie              | *Übung      | 2      | 3     | Wahl-
                                                            pflicht   | Portfolio-Leistungen    | unbenotet        | -           |&
\| Einführung in die
  Interpretation
  philosophischer Texte    | *Übung      | 2      | 3     | Wahl-
                                                            pflicht   | Portfolio-Leistungen    | unbenotet        | -           |&
\| <*Weitere Angaben> {nl}
  Die Studierenden wählen eine der beiden Übungen. {nl}
  *=Anwesenheitspflicht                                                                                             |<|<|<|<|<|<|<|&
\|* <*PHF-phil-BA2>         |* <*Geschichte der Philosophie>                                                           |<|<|<|<|<|<|&
\| <*Semesterlage>          | <*Dauer>                 |<|<| <*Status> | <*Zugangsvoraussetzung>| <*LP/Workload>                 |<|&
\| 1. und 2. Semester       | 2 Semester               |<|<| Pflicht   | -                      | 6 LP / 180 Stunden             |<|&
\| <*Lehrveranstaltunge(n)> | <*Lehrform> | <*SWS> | <*LP> | <*Status> | <*Prüfungsleistungen>  | <*Bewertungsart>   | <*Wichtung> |&
\| Zentrale Themen der
  Philosophie der Antike
  / des Mittelalters       |Seminar      | 2      | 2     | Pflicht   | Protokoll              | unbenotet          | -           |&
\| Zentrale Themen der
  Philosophie der Neuzeit
  / des 20. Jahrhunderts   |Seminar      | 2      | 2     | Pflicht   | Protokoll              | unbenotet          | -           |&
\| <*Weitere Angaben> {nl}
  Die Wahl der Epoche ist mit der Anmeldung zu den Prüfungen verbindlich.                                           |<|<|<|<|<|<|<|&
\|* <*PHF-phil-BA3>         |* <*Einführung in die Theoretische Philosophie>                                           |<|<|<|<|<|<|&
\| <*Semesterlage>          | <*Dauer>                 |<|<| <*Status> | <*Zugangsvoraussetzung>| <*LP/Workload>                 |<|&
\| 1. und 2. Semester       | 1 Semester               |<|<| Pflicht   | -                      | 5 LP / 150 Stunden             |<|&
\| <*Lehrveranstaltunge(n)> | <*Lehrform> | <*SWS> | <*LP> | <*Status> | <*Prüfungsleistungen>  | <*Bewertungsart>   | <*Wichtung> |&
\| Einführung in die
  theoretische Philosophie |Vorlesung    | 2      | 2     | Pflicht   | Take-home-Klausur (ca.
                                                                        5 Seiten) im Rahmen
                                                                        des Seminars           | unbenotet          | -           |&
\| Einführung in die
  theoretische Philosophie |Seminar      | 2      | 3     | Pflicht   | ^                      | ^                  | ^           |&

Example 2:

// headings
\|*
\|* <*Bereich>
\|* <*Modul~(Modulcode)>~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
\|* <*SWS und Veranstaltungs-form>
\|* <*Prüfungs-leistung>{^:fn1}
\|* <*LP {nl} Modul>
\|* <*LP Bereich>
\|&
// Pflicht
// VWL
\|* Pflicht-bereich
\| VWL
\| EInführung in die Volkswirtschaftslehre (VWL-EVWL)
\| 4V+2Ü
\| K
\| 10
\| 35
\|&
\| ^
\| ^
\| Grundzüge der mikroökonomischen Theorie I (VWLvwlMikro1-01a)
\| 2V+ 1-2Ü
\| K
\| 5
\| ^
\|&
\| ^
\| ^
\| Grundzüge der mikroökonomischen Theorie II (VWLvwlMikro1-02a)
\| 2V+ 1-2Ü
\| K
\| 5
\| ^
\|&
\| ^
\| ^
\| Grundzüge der makroökonomischen Theorie I (VWLvwlMakro1-01a)
\| 2V+ 1-2Ü
\| K
\| 5
\| ^
\|&
\| ^
\| ^
\| Grundzüge der makroökonomischen Theorie II (VWLvwlMakro1-02a)
\| 2V+ 1-2Ü
\| K
\| 5
\| ^
\|&
\| ^
\| ^
\| Einführung in die Wirtschaftspolitik (VWLvwlEiWiPo-01a)
\| 2V
\| K
\| 5
\| ^
\|&
// BWL
\| ^
\| BWL
\| Grundlagen der Betriebswirtschaftslehre (BWL-GrundBWL)
\| 2V+1Ü
\| K
\| 5
\| 20
\|&
\| ^
\| ^
\| Buchführung und Abschnluss (BWL-BA)
\| 2V+ 1Ü
\| K
\| 5
\| ^
\|&
\| ^
\| ^
\| Jahresabschluss (BWL-JA)
\| 2V+ 1Ü
\| K
\| 5
\| ^
\|&
\| ^
\| ^
\| Grundlagen der Finanzwirtschaft (BWL-Fiwi1)
\| 2V+ 1Ü
\| K
\| 5
\| ^
\|&
// Mathe etc
\| ^
\| Mathematik, Statistik und Ökonometrie
\| Mathematik I (VWL-MATH1)
\| 2V+2Ü
\| K
\| 5
\| 35
\|&
\| ^
\| ^
\| Mathematik II (VWL-MATH2)
\| 2V+ 2Ü
\| K
\| 5
\| ^
\|&
\| ^
\| ^
\| Methodenlehre der Statistik I (VWL-STAT1)
\| 4V+ 2Ü
\| K
\| 10
\| ^
\|&
\| ^
\| ^
\| Methodenlehre der Statistik II (VWL-STAT2)
\| 4V + 2Ü + 1PC
\| K
\| 10
\| ^
\|&
\| ^
\| ^
\| Einführung in die Ökonometrie (VWL-EIÖK)
\| 2V + 1Ü + 1PC
\| K
\| 5
\| ^
\|&
// Wiss. Arbeiten
\| ^
\| Wiss. Arbeiten
\| Wissenschaftliches Arbeiten (VWLwiWiAr-01a)
\| 2Ü
\| MP
\| 5
\| 5
\|&
// Wahlfplichtbereich
// VWL
\|* Wahl-pflicht-bereich
\| VWL
\| Wahlpflichtmodul 1
\| 2V + 0-2Ü
\| MP
\| 5
\| 40 bis 60
\|&
\| ^
\| ^
\| Wahlpflichtmodul 2
\| 2V + 0-2Ü
\| MP
\| 5
\| ^
\|&
\| ^
\| ^
\| Wahlpflichtmodul 3
\| 2V + 0-2Ü
\| MP
\| 5
\| ^
\|&
\| ^
\| ^
\| Wahlpflichtmodul 4
\| 2V + 0-2Ü
\| MP
\| 5
\| ^
\|&
\| ^
\| ^
\| Wahlpflichtmodul 5
\| 2V + 0-2Ü
\| MP
\| 5
\| ^
\|&
\| ^
\| ^
\| Wahlpflichtmodul 6 oder Seminar 3
\| 2V + 0-2Ü oder 2 S{^:fn2}
\| MP oder S
\| 5
\| ^
\|&
\| ^
\| ^
\| Seminar 1
\| 2 S{^:fn2}
\| S
\| 5
\| ^
\|&
\| ^
\| ^
\| Seminar 2
\| 2 S{^:fn2}
\| S
\| 5
\| ^
\|&
// BWL
\| ^
\| BWL
\| Wahlpflichtmodul 1
\| 2V + 0-2Ü
\| MP
\| 5
\| 10 bis 20{^:fn4}
\|&
\| ^
\| ^
\| Wahlpflichtmodul 2
\| 2V + 0-2Ü
\| MP
\| 5
\| ^
\|&
// Datenanalyse
\| ^
\| Datenanalyse
\| Wahlpflichtmodul
\| 2V + 0-2Ü + 0-1PC
\| MP
\| 5
\| 5 bist 10
\|&
// Ergänzungsbereich
\| ^
\| Ergänzungsbereich
\| Die wählbaren Module werden rechtzeitig und in geeigneter Weise bekannt gemacht.
\| <
\| <
\| verschieden
\| 0 bis 20{^:fn4}
\|&
\| Bachelorarbeit {^:fn3}
\| <
\| <
\| <
\| <
\| <
\| 10
\|&
\| Summe
\| <
\| <
\| <
\| <
\| <
\| 180
\|&

\^{fn1:} Die am schlechtesten bewerteten Module im Umfang von maximal 10 Leistungspunkten, mit Ausnahme der  Bachelorarbeit, werden in unbenotete Module umgewandelt, gemäß § 15 Abs. 3.

\^{fn2:} Regelmäßige Teilnahme gemäß § 12 Abs. 1 ist erforderlich

\^{fn3:} Für Zulassungsvoraussetzungen zur Bachelorarbeit siehe §14.

\^{fn4:} In Wahlfplichtbereich und Ergänzungsbereich dürfen insgesamt nicht mehr als 20 LP an BWL-Modulen erbracht werden.

-}
