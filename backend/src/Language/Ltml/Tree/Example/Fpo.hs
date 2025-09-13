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
            Tree Nothing []

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
