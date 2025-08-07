module Language.Ltml.ToLaTeX (
    generatePDFfromOther,
    --generatePDFfromDocument
) where

import System.Process ( CreateProcess(cwd), readCreateProcessWithExitCode, shell ) 
import System.Exit (ExitCode(..))
import Language.Ltml.ToLaTeX.ToLaTeXM
import Language.Ltml.ToLaTeX.GlobalState (GlobalState(GlobalState, labelToRef))
import Language.Ltml.AST.Document (Document)
import Control.Monad.State (runState)
import Language.Ltml.ToLaTeX.Format (staticDocumentFormat)
import Data.Text (Text)
import Language.Ltml.ToLaTeX.Renderer (renderLaTeX)
import Language.Ltml.ToLaTeX.Type (document)
import qualified Data.Text.Lazy.IO as LTIO
import Text.Megaparsec (runParser, empty, errorBundlePretty)
import Language.Ltml.Parser.Section (sectionP)
import Language.Lsd.Example.Fpo (superSectionT)

initialGlobalState :: GlobalState
initialGlobalState = GlobalState 0 0 0 0 [0]
                                 False False mempty mempty

documentToLaTeX :: FilePath -> Document -> IO ()
documentToLaTeX filename doc = 
    let (latexDoc, gs) = runState (toLaTeXM doc) initialGlobalState
        texPath = "./src/Language/Ltml/ToLaTeX/Auxiliary/" <> filename
     in LTIO.writeFile texPath $ renderLaTeX (labelToRef gs) latexDoc

otherToLaTeX :: ToLaTeXM a => FilePath -> a -> IO ()
otherToLaTeX filename sec = 
    let (latexSection, gs) = runState (toLaTeXM sec) initialGlobalState
        texPath = "./src/Language/Ltml/ToLaTeX/Auxiliary/" <> filename
     in LTIO.writeFile texPath $ 
            renderLaTeX (labelToRef gs) 
                        (staticDocumentFormat <> document latexSection)

-- | TODO: what to do with auxiliary files and multiple requests in case of asynchronicity
mkPDF ::FilePath -> IO (Either String ())
mkPDF filename = do
    let 
        pdfCommand = "pdflatex -interaction=nonstopmode -halt-on-error " <> filename
        workingDir = "./src/Language/Ltml/ToLaTeX/Auxiliary"

    -- Run pdflatex and capture output
    (exitCode, stdout, _) <- readCreateProcessWithExitCode 
        (shell pdfCommand) { cwd = Just workingDir } ""

    case exitCode of
        ExitSuccess -> do 
            putStrLn "PDF generated successfully."
            return $ Right ()
        ExitFailure _ -> do
            putStrLn "LaTeX compilation failed."
            return $ Left $ drop 3094 stdout -- omitting the preambel of the pdflatex output here. 
                                             -- could be different on another system and thus maybe revert later

generatePDFfromOther :: Text -> IO (Either String ())
generatePDFfromOther input = 
    case runParser (sectionP superSectionT empty) "" input of
        Left err -> return $ Left (errorBundlePretty err)
        Right parsedInput -> do
            let outputfile = "output.tex"
            otherToLaTeX outputfile parsedInput
            mkPDF outputfile

-- generatePDFfromDocument :: Text -> IO (Either String ())
-- generatePDFfromDocument input = 
--     case runParser (sectionP mainDocT empty) "" input of
--         Left err -> return $ Left (errorBundlePretty err)
--         Right parsedInput -> do
--             let outputfile = "output.tex"
--             documentToLaTeX outputfile parsedInput
--             mkPDF outputfile