module Language.Ltml.ToLaTeX (
    mkPDFwithDocument,
    mkPDFwithOther,
) where

import System.Process ( createProcess, proc, CreateProcess(cwd) ) 
import Language.Ltml.ToLaTeX.ToLaTeXM
import Language.Ltml.ToLaTeX.GlobalState (GlobalState(GlobalState, labelToRef))
import Language.Ltml.AST.Document (Document)
import Control.Monad.State (runState)
import Language.Ltml.ToLaTeX.Format (staticDocumentFormat)
import Data.Text.Lazy (Text)
import Language.Ltml.ToLaTeX.Renderer (renderLaTeX)
import Language.Ltml.ToLaTeX.Type (document)
import qualified Data.Text.Lazy.IO as TLIO
import System.IO.Silently ( silence )

initialGlobalState :: GlobalState
initialGlobalState = GlobalState 0 0 0 0 
                                 False False mempty mempty

documentToLaTeX :: Document -> Text
documentToLaTeX doc = let (latexDoc, gs) = runState (toLaTeXM doc) initialGlobalState
                      in renderLaTeX (labelToRef gs) latexDoc

otherToLaTeX :: ToLaTeXM a => a -> Text
otherToLaTeX sec = let (latexSection, gs) = runState (toLaTeXM sec) initialGlobalState
                     in renderLaTeX (labelToRef gs) (staticDocumentFormat <> document latexSection)

outputFile :: FilePath
outputFile = "./src/Language/Ltml/ToLaTeX/Auxiliary/output.tex"

workingDir :: FilePath
workingDir = "./src/Language/Ltml/ToLaTeX/Auxiliary"

mkPDFwithDocument :: Document -> IO ()
mkPDFwithDocument doc = do
    TLIO.writeFile outputFile (documentToLaTeX doc)
    let procSpec = (proc "pdflatex" ["output.tex"]) { cwd = Just workingDir }
    _ <- createProcess procSpec 
    pure ()

mkPDFwithOther :: ToLaTeXM a => FilePath ->  a -> IO ()
mkPDFwithOther filename doc = silence $ do
    TLIO.writeFile ("./src/Language/Ltml/ToLaTeX/Auxiliary/" <> filename) (otherToLaTeX doc)
    let procSpec = (proc "pdflatex" [filename]) { cwd = Just workingDir }
    _ <- createProcess procSpec
    pure ()

-- generatePDF :: Text -> IO ()