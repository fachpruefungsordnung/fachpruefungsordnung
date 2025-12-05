{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Tree.Example.Fpo
    ( fpoTree
    )
where

import Data.Text (Text, unlines)
import Language.Ltml.Common (Flagged (Flagged))
import Language.Ltml.Tree
    ( FlaggedInputTree'
    , Tree (Leaf, Tree)
    , TypedInputTree'
    , TypedTree (TypedTree)
    )
import Prelude hiding (unlines)

fpoTree :: FlaggedInputTree'
fpoTree =
    Flagged False $
        TypedTree "document-container" "fpo-container" $
            Tree (Just header) (mainDocTree : appTrees)
  where
    header :: Text
    header =
        unlines
            [ "// Document container header."
            , "// The order of entries is fixed (TODO)."
            , ""
            , "pdf-title: Beispieltitel"
            , ""
            , "// Name of the institution here."
            , "header-footer-supertitle: Universität BeispielStadt"
            , ""
            , "header-footer-title: FPO Beispiel 2025"
            , ""
            , "header-footer-date: 2025-08-26"
            ]

mainDocTree :: FlaggedInputTree'
mainDocTree =
    Flagged False $
        TypedTree "document" "fpo-maindoc" $
            Tree
                (Just heading)
                [introTree, mainBodyTree, extroTree]
  where
    heading =
        unlines
            [ "// Only the document's heading is permitted here."
            , "! Beispiel-Überschrift"
            ]

    introTree =
        Flagged False $
            TypedTree "document-intro" "" $
                Leaf introText

    mainBodyTree =
        Flagged False $
            TypedTree "document-mainbody" "fpo-mainbody-simple" $
                Tree Nothing [Flagged True sampleSectionTree]

    extroTree =
        Flagged False $
            TypedTree "document-extro" "" $
                Leaf extroText

appTrees :: [FlaggedInputTree']
appTrees = [appendixTree, attachmentsTree]

appendixTree :: FlaggedInputTree'
appendixTree =
    Flagged False $
        TypedTree "appendix-section" "appendix" $
            Tree Nothing [Flagged False appendixDocument]
  where
    appendixDocument :: TypedInputTree'
    appendixDocument =
        TypedTree "document" "simpledoc" $
            Tree
                (Just appendixHeading)
                [Flagged False appendixDocumentBody]

    appendixDocumentBody :: TypedInputTree'
    appendixDocumentBody =
        TypedTree "document-mainbody" "simpledoc-mainbody" $
            Leaf simpleBlockText

    appendixHeading :: Text
    appendixHeading =
        unlines
            [ "// Only the appendix document's heading is permitted here."
            , "! Beispiel Anhang"
            ]

attachmentsTree :: FlaggedInputTree'
attachmentsTree =
    Flagged False $
        TypedTree "appendix-section" "attachments" $
            Tree Nothing []

introText :: Text
introText =
    unlines
        [ "[date]"
        , ""
        , "// Insert correct date here; displayed at the very top."
        , "Vom 19. Januar 2038"
        , ""
        , ""
        , "[publ_log]"
        , ""
        , "// Publication log; typically one paragraph (i.e., no empty lines)"
        , "// with many comma-separated \"Veröffentlichung vom ...\""
        , ""
        , ""
        , "[intro]"
        , ""
        , "// Intro; typically one paragraph à la:"
        , "// \"Aufgrund ... wird ... erlassen:\""
        ]

sampleSectionTree :: TypedInputTree'
sampleSectionTree =
    TypedTree "section" "section" $
        Leaf sampleSectionText

sampleSectionText :: Text
sampleSectionText =
    unlines
        [ "// document heading; mandatory, with the § sign."
        , "§ section title"
        , ""
        , "// Section body -- a sequence of paragraphs, separated by empty"
        , "// lines."
        , ""
        , "This is a simple paragraph."
        , ""
        , "This is another paragraph, with an enumeration:"
        , "  # first item"
        , "  # second item"
        , "      # sub-item"
        , "  # third item, which"
        , "    spans several lines"
        , ""
        , "{some_paragraph:}"
        , "A labeled paragraph, referencing itself: {:some_paragraph}."
        , "It also has a footnote{^:a_footnote}."
        , "{a_sentence:} It also has a self-referencing sentence:"
        , "{:a_sentence}."
        , ""
        , "// A footnote is defined as follows:"
        , "^{a_footnote:}"
        , "  The footnote."
        ]

extroText :: Text
extroText =
    unlines
        [ "[extro]"
        , ""
        , "// Here goes some text that goes at the end of the document."
        , ""
        , "// In particular, there should be the date and name of the"
        , "// signatory, with space for the actual signature."
        , "//  - Vertical space can be inserted as {nl}."
        , ""
        , ""
        , "[legal_log]"
        , ""
        , "// Here, a sequence of paragraphs like the following should go."
        , ""
        , "<*Artikel 42 der Änderungssatzung vom 19. Januar 2038:>"
        , "Diese Satzung tritt am Tag nach ihrer Bekanntmachung in Kraft."
        ]

simpleBlockText :: Text
simpleBlockText =
    unlines
        [ "// Here goes all text regarding this appendix section."
        , "// An appendix section consists of regular paragraphs and tables."
        , -- , "// For better readability, an appendix section"
          -- , "// should contain only a single table."
          ""
        , "This an appendix section. Here, many useful pieces of information are specified."
        , ""
        , "// A simple table could look like this:"
        , ""
        , "| A | B |&"
        , "| C | D |&"
        , ""
        , "// The '&' symbol is used to mark the end of a row."
        , ""
        , "// Cells can also be merged across rows and columns"
        , "// using the arrow symbols '<' and '^':"
        , ""
        , "Vertically merged cells look like this: "
        , ""
        , "| A | < |&"
        , "| C | D |&"
        , ""
        , "Horizontally merged cells look like this:"
        , ""
        , "| A | B |&"
        , "| ^ | D |&"
        , ""
        , ""
        , "// There is also a special syntax for module definitions and studyplans:"
        , "The following is a simple list of module definitions."
        , ""
        , "// First, a schema is defined:"
        , "schema: Name | ECTS | Score"
        , "// A list of modules follows:"
        , "module: ABC | 8 | A"
        , "module: DEFG | 4 | B"
        , ""
        , "Furthermore, a studyplan is defined."
        , ""
        , "// Additionally modules can be grouped into categories:"
        , "schema: Name | ECTS | Score"
        , "category: 1. Semester"
        , "module: ABC | 8 | A"
        , "module: DEFG | 4 | B"
        , ""
        , "category: 2. Semester"
        , "module: ABC II | 5-8 | "
        , "module: D I | | C{^:c_grade}"
        , ""
        , "^{c_grade:} A footnote!"
        ]
