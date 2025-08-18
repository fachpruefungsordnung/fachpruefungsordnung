{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.Example.Fpo
    ( sampleDocIntroText
    , sampleSectionText
    , sampleDocExtroText
    )
where

import Data.Text (Text, unlines)
import Prelude hiding (unlines)

sampleDocIntroText :: Text
sampleDocIntroText =
    unlines
        [ "[date]"
        , ""
        , "// Insert correct date here; displayed at the very top."
        , "//  - For now, the date must be made bold explicitly here (TODO)."
        , "<*Vom 19. Januar 2038>"
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

sampleDocExtroText :: Text
sampleDocExtroText =
    unlines
        [ "[extro]"
        , ""
        , "// Here goes some text that goes at the end of the document."
        , ""
        , "// In particular, there should be the date and name of the"
        , "// signatory, with space for the actual signature."
        , "//  - Adding vertical space is currently not possible (TODO)."
        , ""
        , ""
        , "[legal_log]"
        , ""
        , "// Here, a sequence of paragraphs like the following should go."
        , ""
        , "<*Artikel 42 der Änderungssatzung vom 19. Januar 2038:>"
        , "Diese Satzung tritt am Tag nach ihrer Bekanntmachung in Kraft."
        ]
