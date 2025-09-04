{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.TestError (err) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (fromList)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

err :: ParseErrorBundle Text Void
err =
    ParseErrorBundle
        { bundleErrors =
            TrivialError 49 (Just (Tokens ('s' :| ""))) (fromList [Tokens ('\167' :| "")])
                :| []
        , bundlePosState =
            PosState
                { pstateInput =
                    "// document heading; mandatory, with the \167 sign.\nsection title\n\n// Section body -- a sequence of paragraphs, separated by empty\n// lines.\n\nThis is a simple paragraph.\n\nThis is another paragraph, with an enumeration:\n  # first item\n  # second item\n      # sub-item\n  # third item, which\n    spans several lines\n\n{some_paragraph:}\nA labeled paragraph, referencing itself: {:some_paragraph}.\nIt also has a footnote{^:a_footnote}.\n{a_sentence:} It also has a self-referencing sentence:\n{:a_sentence}.\n\n// A footnote is defined as follows:\n^{a_footnote:}\n  The footnote.\n\n"
                , pstateOffset = 0
                , pstateSourcePos =
                    SourcePos {sourceName = "", sourceLine = mkPos 1, sourceColumn = mkPos 1}
                , pstateTabWidth = mkPos 8
                , pstateLinePrefix = ""
                }
        }
