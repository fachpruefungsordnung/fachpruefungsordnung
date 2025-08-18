{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Testing
    ( testThis
    , readText
    , runTestToPDF
    , runTestToLaTeX
    , superSectionWithNSubsections
    , hugeSuperSection
    , tmp
    )
where

import Control.Monad.State (runState)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LTIO
import Language.Lsd.AST.Format
    ( EnumStyle (Arabic)
    , FormatAtom (PlaceholderAtom, StringAtom)
    , FormatString (FormatString)
    , HeadingPlaceholderAtom (HeadingTextPlaceholder, IdentifierPlaceholder)
    , ParagraphKeyFormat (ParagraphKeyFormat)
    , TocKeyFormat (TocKeyFormat)
    )
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (ParagraphFormat))
import Language.Lsd.AST.Type.Section (SectionFormat (SectionFormat))
import Language.Lsd.Example.Fpo (footnoteT, sectionT)
import Language.Ltml.AST.Label (Label (Label))
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Paragraph (Paragraph (Paragraph))
import Language.Ltml.AST.Section
    ( Heading (Heading)
    , Section (Section)
    , SectionBody (InnerSectionBody, LeafSectionBody)
    )
import Language.Ltml.AST.Text (TextTree (Reference, Space, Word))
import Language.Ltml.Parser.Footnote (unwrapFootnoteParser)
import Language.Ltml.Parser.Section (sectionP)
import Language.Ltml.ToLaTeX (generatePDFFromSection)
import Language.Ltml.ToLaTeX.Format (staticDocumentFormat)
import Language.Ltml.ToLaTeX.GlobalState
    ( GlobalState (GlobalState, labelToFootNote, labelToRef)
    )
import Language.Ltml.ToLaTeX.Renderer (renderLaTeX)
import Language.Ltml.ToLaTeX.ToLaTeXM (ToLaTeXM (toLaTeXM))
import Language.Ltml.ToLaTeX.Type
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, runParser)

import System.IO

readText :: String -> Text
readText filename = unsafePerformIO $ TIO.readFile filename

initialState :: GlobalState
initialState =
    GlobalState
        0
        0
        0
        0
        [0]
        (FormatString [])
        False
        False
        mempty
        mempty
        0
        mempty

testThis :: (ToLaTeXM a) => a -> (LaTeX, GlobalState)
testThis a =
    runState
        (toLaTeXM a)
        initialState

superSectionWithNSubsections :: Int -> Node Section
superSectionWithNSubsections n =
    Node (Just $ Label "super") $
        Section
            ( SectionFormat
                (FormatString [PlaceholderAtom Arabic])
                (TocKeyFormat (FormatString []))
            )
            ( Heading
                ( FormatString
                    [ StringAtom "§ "
                    , PlaceholderAtom IdentifierPlaceholder
                    , StringAtom "\n"
                    , PlaceholderAtom HeadingTextPlaceholder
                    ]
                )
                [ Word "This"
                , Space
                , Word "is"
                , Space
                , Word "a"
                , Space
                , Word "random"
                , Space
                , Word "super"
                , Space
                , Word "heading"
                ]
            )
            ( LeafSectionBody $
                replicate
                    n
                    ( Node
                        Nothing
                        ( Paragraph
                            ( ParagraphFormat
                                (FormatString [PlaceholderAtom Arabic])
                                (ParagraphKeyFormat (FormatString []))
                            )
                            [ Word "This"
                            , Space
                            , Word "phrase"
                            , Space
                            , Word "refers"
                            , Space
                            , Word "to"
                            , Space
                            , Word "the"
                            , Space
                            , Word "section"
                            , Space
                            , Reference (Label "super")
                            ]
                        )
                    )
            )

hugeSuperSection :: Int -> Section
hugeSuperSection n =
    Section
        ( SectionFormat
            (FormatString [PlaceholderAtom Arabic])
            (TocKeyFormat (FormatString []))
        )
        ( Heading
            ( FormatString
                [ StringAtom "Supersection "
                , PlaceholderAtom IdentifierPlaceholder
                , StringAtom " "
                , PlaceholderAtom HeadingTextPlaceholder
                ]
            )
            [ Word "This"
            , Space
            , Word "is"
            , Space
            , Word "a"
            , Space
            , Word "random"
            , Space
            , Word "super"
            , Space
            , Word "heading"
            ]
        )
        (InnerSectionBody $ replicate n (superSectionWithNSubsections n))

runTestToPDF :: IO ()
runTestToPDF = do
    let txt = readText "./src/Language/Ltml/ToLaTeX/Auxiliary/test.txt"
    eAction <- generatePDFFromSection txt
    case eAction of
        Left err -> error err
        Right pdf -> BS.writeFile "./src/Language/Ltml/ToLaTeX/Auxiliary/test.pdf" pdf

runTestToLaTeX :: IO String
runTestToLaTeX = do
    let input = readText "./src/Language/Ltml/ToLaTeX/Auxiliary/test.txt"
    case runParser (unwrapFootnoteParser [footnoteT] (sectionP sectionT eof)) "" input of
        Left err -> return (errorBundlePretty err)
        Right parsedInput -> do
            let texFile = "./src/Language/Ltml/ToLaTeX/Auxiliary/test.tex"
            -- Write LaTeX source
            LTIO.writeFile texFile (sectionToText parsedInput)
            return "everything went well!"
  where
    sectionToText (sec, labelmap) =
        let (latexSection, gs) = runState (toLaTeXM sec) $ initialState {labelToFootNote = labelmap}
         in renderLaTeX (labelToRef gs) (staticDocumentFormat <> document latexSection)

tmp :: IO ()
tmp = do
    h <- openFile "./src/Language/Ltml/ToLaTeX/Auxiliary/test.tex" ReadMode
    hSetEncoding h utf8
    contents <- hGetContents h
    putStrLn contents
