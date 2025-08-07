{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.ToLaTeX.Testing
    (testThis,
     readText,
     runTest,
     superSectionWithNSubsections,
     hugeSuperSection,
     getTestSection)
where

import Text.Megaparsec ( empty, runParser )
import Language.Lsd.Example.Fpo (superSectionT)
import Language.Ltml.Parser.Section (sectionP)
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import System.IO.Unsafe (unsafePerformIO)
import Language.Ltml.ToLaTeX.Type
import Language.Ltml.ToLaTeX.ToLaTeXM (ToLaTeXM(toLaTeXM))
import Control.Monad.State (runState)
import Language.Ltml.AST.Section (Section (Section), Heading (Heading))
import Language.Lsd.AST.Type.Section (SectionFormat(SectionFormat))
import Language.Lsd.AST.Format (FormatAtom(PlaceholderAtom, StringAtom), EnumStyle (Arabic), HeadingPlaceholderAtom (IdentifierPlaceholder, HeadingTextPlaceholder), FormatString (FormatString))
import Language.Ltml.AST.Node (Node(Node))
import Language.Ltml.AST.Paragraph (Paragraph(Paragraph))
import Language.Lsd.AST.Type.Paragraph (ParagraphFormat(ParagraphFormat))
import Language.Ltml.AST.Text (TextTree(Word, Space, Reference))
import Language.Ltml.AST.Label (Label(Label))
import Language.Ltml.ToLaTeX (generatePDFFromSection)
import qualified Data.ByteString.Lazy as BS
import Language.Ltml.ToLaTeX.GlobalState (GlobalState (GlobalState))

readText :: String -> Text
readText filename = unsafePerformIO $ TIO.readFile filename

initialState :: GlobalState
initialState = GlobalState 0 0 0 0 [0] False False mempty mempty

getTestSection :: Node Section
getTestSection = 
            either undefined id
            $ runParser (sectionP superSectionT empty) "" 
                (readText "./src/Language/Ltml/ToLaTeX/Auxiliary/test.txt")

testThis :: ToLaTeXM a => a -> (LaTeX, GlobalState)
testThis a = runState (toLaTeXM a)
            initialState

superSectionWithNSubsections :: Int -> Node Section
superSectionWithNSubsections n = 
    Node (Just $ Label "super") $ 
          Section
            (SectionFormat (FormatString [PlaceholderAtom Arabic]))
            (Heading 
                    (FormatString [
                                    StringAtom "§ ",
                                    PlaceholderAtom IdentifierPlaceholder,
                                    StringAtom "\n",
                                    PlaceholderAtom HeadingTextPlaceholder
                                ])
                    [
                        Word "This", Space, 
                        Word "is", Space, 
                        Word "a", Space, 
                        Word "random", Space, 
                        Word "super", Space, 
                        Word "heading"
                    ])
            (Left $ replicate n (Node
                                    Nothing
                                    (Paragraph
                                            (ParagraphFormat (FormatString [PlaceholderAtom Arabic]))
                                            [
                                                Word "This", Space, 
                                                Word "phrase", Space, 
                                                Word "refers", Space, 
                                                Word "to", Space, 
                                                Word "the", Space, 
                                                Word "section", Space, 
                                                Reference (Label "super")
                                            ]
                                    )
                                )   
            )

hugeSuperSection :: Int -> Section
hugeSuperSection n = Section
                        (SectionFormat (FormatString [PlaceholderAtom Arabic]))
                        (Heading 
                                (FormatString [
                                                StringAtom "Supersection ",
                                                PlaceholderAtom IdentifierPlaceholder,
                                                StringAtom " ",
                                                PlaceholderAtom HeadingTextPlaceholder
                                            ])
                                [
                                    Word "This", Space, 
                                    Word "is", Space, 
                                    Word "a", Space, 
                                    Word "random", Space, 
                                    Word "super", Space, 
                                    Word "heading"
                                ])
                        (Right $ replicate n (superSectionWithNSubsections n))



runTest :: IO ()
runTest = do
    let txt = readText "./src/Language/Ltml/ToLaTeX/Auxiliary/test.txt"
    eAction <- generatePDFFromSection txt
    case eAction of
        Left err -> error err
        Right pdf -> BS.writeFile "./src/Language/Ltml/ToLaTeX/Auxiliary/test.pdf" pdf


