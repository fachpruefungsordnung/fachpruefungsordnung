{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Testing
    ( testThis
    , readText
    , runTestToPDF
    , runTestToLaTeX
    , testingDocumentContainer
    , startTesting
    )
where

import Control.Lens (view)
import Control.Monad.State (runState)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text.Lazy.IO as LTIO
import Data.Typography
    ( FontSize (MediumFontSize, SmallFontSize)
    , FontStyle (Bold)
    , TextAlignment (Centered, LeftAligned)
    , Typography (Typography)
    )
import Language.Lsd.AST.Format
    ( EnumStyle (AlphabeticUpper, Arabic)
    , FormatAtom (PlaceholderAtom, StringAtom)
    , FormatString (FormatString)
    , HeadingFormat (HeadingFormat)
    , HeadingPlaceholderAtom (HeadingTextPlaceholder, IdentifierPlaceholder)
    , KeyPlaceholderAtom (KeyIdentifierPlaceholder)
    , ParagraphKeyFormat (ParagraphKeyFormat)
    , TocKeyFormat (TocKeyFormat)
    )
import Language.Lsd.AST.Type (NamedType (NamedType))
import Language.Lsd.AST.Type.AppendixSection
    ( AppendixElementFormat (AppendixElementFormat)
    , AppendixSectionFormat (AppendixSectionFormat)
    , AppendixSectionTitle (AppendixSectionTitle)
    )
import Language.Lsd.AST.Type.Document (DocumentFormat (DocumentFormat))
import Language.Lsd.AST.Type.DocumentContainer
    ( DocumentContainerFormat (DocumentContainerFormat)
    , HeaderFooterFormat (HeaderFooterFormat)
    , HeaderFooterFormatAtom
        ( HeaderFooterCurPageNumAtom
        , HeaderFooterDateAtom
        , HeaderFooterLastPageNumAtom
        , HeaderFooterSuperTitleAtom
        , HeaderFooterTitleAtom
        )
    , HeaderFooterItemFormat (HeaderFooterItemFormat)
    )
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat (ParagraphFormat))
import Language.Lsd.AST.Type.Section (SectionFormat (SectionFormat))
import Language.Lsd.Example.Fpo (footnoteT, sectionT)
import Language.Ltml.AST.AppendixSection (AppendixSection (AppendixSection))
import Language.Ltml.AST.Document
    ( Document (Document)
    , DocumentBody (DocumentBody)
    , DocumentHeading (DocumentHeading)
    )
import Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (DocumentContainer)
    , DocumentContainerHeader (DocumentContainerHeader)
    )
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Paragraph (Paragraph (Paragraph))
import Language.Ltml.AST.Section
    ( Heading (Heading)
    , Section (Section)
    , SectionBody (InnerSectionBody, LeafSectionBody)
    )
import Language.Ltml.AST.Text (TextTree (Space, Word))
import Language.Ltml.Common (Flagged (Flagged))
import Language.Ltml.Parser.Common.Lexeme (nSc)
import Language.Ltml.Parser.Footnote (runFootnoteWriterT)
import Language.Ltml.Parser.Section (sectionP)
import Language.Ltml.ToLaTeX (generatePDFFromSection)
import Language.Ltml.ToLaTeX.GlobalState
    ( GlobalState (_labelToFootNote)
    , initialGlobalState
    , labelToRef
    , preDocument
    )
import Language.Ltml.ToLaTeX.PreLaTeXType
import Language.Ltml.ToLaTeX.Renderer (renderLaTeX)
import Language.Ltml.ToLaTeX.ToLaTeX (toLaTeX)
import Language.Ltml.ToLaTeX.ToPreLaTeXM (ToPreLaTeXM (toPreLaTeXM))
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (MonadParsec (eof), errorBundlePretty, runParser)

readText :: String -> Text
readText filename = unsafePerformIO $ TIO.readFile filename

testThis :: (ToPreLaTeXM a) => a -> (PreLaTeX, GlobalState)
testThis a =
    runState
        (toPreLaTeXM a)
        initialGlobalState

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
        NamedType _ _ sectionT' = sectionT
        NamedType _ _ footnoteT' = footnoteT
    case runParser
        (nSc *> runFootnoteWriterT (sectionP sectionT' eof) [footnoteT'])
        ""
        (input <> "\n") of
        Left err -> return (errorBundlePretty err)
        Right parsedInput -> do
            let texFile = "./src/Language/Ltml/ToLaTeX/Auxiliary/test.tex"
            -- Write PreLaTeX source
            LTIO.writeFile texFile (sectionToText parsedInput)
            return "everything went well!"
  where
    sectionToText (sec, labelmap) =
        let (latexSection, gs) = runState (toPreLaTeXM sec) $ initialGlobalState {_labelToFootNote = labelmap}
         in renderLaTeX $
                toLaTeX (view labelToRef gs) (view preDocument gs <> document latexSection)

testingParagraph :: Paragraph
testingParagraph =
    Paragraph
        ( ParagraphFormat
            (FormatString [PlaceholderAtom Arabic])
            ( ParagraphKeyFormat $
                FormatString
                    [StringAtom "(", PlaceholderAtom KeyIdentifierPlaceholder, StringAtom ")"]
            )
        )
        [ Word "This"
        , Space
        , Word "is"
        , Space
        , Word "a"
        , Space
        , Word "random"
        , Space
        , Word "paragraph"
        , Space
        , Word "with"
        , Space
        , Word "random"
        , Space
        , Word "content"
        , Space
        ]

testingSection :: Section
testingSection =
    Section
        ( SectionFormat
            (FormatString [PlaceholderAtom Arabic])
            ( TocKeyFormat $
                FormatString [StringAtom "§ ", PlaceholderAtom KeyIdentifierPlaceholder]
            )
        )
        ( Heading
            ( HeadingFormat
                (Typography Centered MediumFontSize [Bold])
                ( FormatString
                    [ PlaceholderAtom IdentifierPlaceholder
                    , StringAtom " - "
                    , PlaceholderAtom HeadingTextPlaceholder
                    ]
                )
            )
            [ Word "This"
            , Space
            , Word "is"
            , Space
            , Word "a"
            , Space
            , Word "random"
            , Space
            , Word "section"
            ]
        )
        (LeafSectionBody [Node Nothing testingParagraph])

testingDocument :: Document
testingDocument =
    Document
        (DocumentFormat True)
        ( DocumentHeading
            [ Word "This"
            , Space
            , Word "is"
            , Space
            , Word "a"
            , Space
            , Word "random"
            , Space
            , Word "document"
            ]
        )
        ( DocumentBody
            (Flagged False [])
            (Flagged False $ InnerSectionBody [Flagged True $ Node Nothing testingSection])
            (Flagged False [])
        )
        mempty

testingAppendixSection :: AppendixSection
testingAppendixSection =
    AppendixSection
        ( AppendixSectionFormat
            (AppendixSectionTitle "\nAppendices")
            ( AppendixElementFormat
                (FormatString [PlaceholderAtom AlphabeticUpper])
                ( TocKeyFormat $
                    FormatString [StringAtom "Appendix ", PlaceholderAtom KeyIdentifierPlaceholder]
                )
                ( HeadingFormat
                    (Typography LeftAligned MediumFontSize [Bold])
                    ( FormatString
                        [ PlaceholderAtom IdentifierPlaceholder
                        , StringAtom " - "
                        , PlaceholderAtom HeadingTextPlaceholder
                        ]
                    )
                )
            )
        )
        [Flagged False $ Node Nothing doc, Flagged False $ Node Nothing doc]
  where
    doc =
        Document
            (DocumentFormat True)
            ( DocumentHeading
                [ Word "This"
                , Space
                , Word "is"
                , Space
                , Word "a"
                , Space
                , Word "random"
                , Space
                , Word "appendix"
                ]
            )
            ( DocumentBody
                (Flagged False [])
                ( Flagged False $
                    InnerSectionBody
                        [ Flagged False $ Node Nothing testingSection
                        , Flagged False $ Node Nothing testingSection
                        , Flagged False $ Node Nothing testingSection
                        ]
                )
                (Flagged False [])
            )
            mempty

testingDocumentContainer :: DocumentContainer
testingDocumentContainer =
    DocumentContainer
        ( DocumentContainerFormat
            ( HeaderFooterFormat
                [ HeaderFooterItemFormat
                    MediumFontSize
                    [Bold]
                    (FormatString [PlaceholderAtom HeaderFooterSuperTitleAtom, StringAtom "\n"])
                , HeaderFooterItemFormat
                    MediumFontSize
                    []
                    (FormatString [PlaceholderAtom HeaderFooterTitleAtom])
                ]
                []
                [ HeaderFooterItemFormat
                    SmallFontSize
                    []
                    (FormatString [StringAtom "(Keine amtliche Bekanntmachung)"])
                ]
            )
            ( HeaderFooterFormat
                [ HeaderFooterItemFormat
                    MediumFontSize
                    []
                    (FormatString [PlaceholderAtom HeaderFooterDateAtom])
                ]
                [ HeaderFooterItemFormat
                    MediumFontSize
                    []
                    (FormatString [StringAtom "centered text"])
                ]
                [ HeaderFooterItemFormat
                    SmallFontSize
                    []
                    ( FormatString
                        [ StringAtom "Seite "
                        , PlaceholderAtom HeaderFooterCurPageNumAtom
                        , StringAtom "/"
                        , PlaceholderAtom HeaderFooterLastPageNumAtom
                        ]
                    )
                ]
            )
            ( HeadingFormat
                (Typography Centered MediumFontSize [Bold])
                (FormatString [PlaceholderAtom HeadingTextPlaceholder])
            )
        )
        ( DocumentContainerHeader
            "pdftitle"
            "This is a random document container"
            "Made with Love"
            "August 22, 2023"
        )
        (Flagged False testingDocument)
        [Flagged False testingAppendixSection]

startTesting :: IO ()
startTesting = do
    let (latex, gs) = testThis testingDocumentContainer
    LT.putStrLn $
        renderLaTeX $
            toLaTeX (view labelToRef gs) (view preDocument gs <> document latex)
