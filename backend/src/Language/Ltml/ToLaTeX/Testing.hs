{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Testing
    ( testThis
    , readText
    , runTest
    , anotherTest
    , superSectionWithNSubsections
    , hugeSuperSection
    , getTestSection
    )
where

import Control.Monad.State (runState)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text.IO as TIO
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
import Language.Lsd.Example.Fpo (superSectionT)
import Language.Ltml.AST.Label (Label (Label))
import Language.Ltml.AST.Node (Node (Node))
import Language.Ltml.AST.Paragraph (Paragraph (Paragraph))
import Language.Ltml.AST.Section
    ( Heading (Heading)
    , Section (Section)
    , SectionBody (InnerSectionBody, LeafSectionBody)
    )
import Language.Ltml.AST.Text (TextTree (Reference, Space, Word))
import Language.Ltml.Parser.Section (sectionP)
import Language.Ltml.ToLaTeX (generatePDFFromSuperSection)
import Language.Ltml.ToLaTeX.GlobalState
    ( GlobalState (GlobalState, toc)
    , fromDList
    )
import Language.Ltml.ToLaTeX.ToLaTeXM (ToLaTeXM (toLaTeXM))
import Language.Ltml.ToLaTeX.Type
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (empty, runParser)

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

getTestSection :: Node Section
getTestSection =
    either undefined id $
        runParser
            (sectionP superSectionT empty)
            ""
            (readText "./src/Language/Ltml/ToLaTeX/Auxiliary/test.txt")

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

runTest :: IO ()
runTest = do
    let txt = readText "./src/Language/Ltml/ToLaTeX/Auxiliary/test.txt"
    eAction <- generatePDFFromSuperSection txt
    case eAction of
        Left err -> error err
        Right pdf -> BS.writeFile "./src/Language/Ltml/ToLaTeX/Auxiliary/test.pdf" pdf

anotherTest :: IO LaTeX
anotherTest = do
    let (_, st) = testThis getTestSection
    return $ Sequence $ fromDList $ toc st
