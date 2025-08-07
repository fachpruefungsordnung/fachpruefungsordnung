module Language.Ltml.ToLaTeX
    ( generatePDFFromSection
    --   generatePDFFromDocument
    ) where

import Control.Monad.State (runState)
import qualified Data.ByteString.Lazy as BS
import Data.Text (Text)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LTIO
import Data.Void (Void)
import Language.Lsd.Example.Fpo (superSectionT)
import Language.Ltml.Parser.Section (sectionP)
import Language.Ltml.ToLaTeX.Format (staticDocumentFormat)
import Language.Ltml.ToLaTeX.GlobalState (GlobalState (GlobalState, labelToRef))
import Language.Ltml.ToLaTeX.Renderer (renderLaTeX)
import Language.Ltml.ToLaTeX.ToLaTeXM
import Language.Ltml.ToLaTeX.Type (document)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process
    ( CreateProcess (cwd)
    , readCreateProcessWithExitCode
    , shell
    )
import Text.Megaparsec (Parsec, empty, errorBundlePretty, runParser)

initialGlobalState :: GlobalState
initialGlobalState =
    GlobalState
        0
        0
        0
        0
        [0]
        False
        False
        mempty
        mempty

generatePDFfromParsed
    :: Parsec Void Text a -> (a -> LT.Text) -> Text -> IO (Either String BS.ByteString)
generatePDFfromParsed parser render input =
    case runParser parser "" input of
        Left err -> return $ Left (errorBundlePretty err)
        Right parsedInput ->
            withSystemTempDirectory "latex-temp" $ \tmpDir -> do
                let texFile = tmpDir </> "input.tex"
                    pdfFile = tmpDir </> "input.pdf"
                    cmd = "pdflatex -interaction=nonstopmode -halt-on-error input.tex"

                -- Write LaTeX source
                LTIO.writeFile texFile (render parsedInput)

                -- Compile with pdflatex
                (exitCode, stdout, _) <-
                    readCreateProcessWithExitCode
                        (shell cmd) {cwd = Just tmpDir}
                        ""

                case exitCode of
                    ExitFailure _ -> do
                        let cleanErr = drop 3094 stdout -- omitting the preambel of the pdflatex output here.
                        -- could be different on another system and thus maybe revert later
                        return $ Left cleanErr
                    ExitSuccess -> do
                        pdf <- BS.readFile pdfFile
                        return $ Right pdf

-- generatePDFFromDocument :: Text -> IO (Either String BS.ByteString)
-- generatePDFFromDocument =
--     generatePDFfromParsed (documentP superSectionT empty) documentToText
--   where
--     documentToText doc =
--         let (latexDoc, gs) = runState (toLaTeXM doc) initialGlobalState
--          in renderLaTeX (labelToRef gs) latexDoc

generatePDFFromSection :: Text -> IO (Either String BS.ByteString)
generatePDFFromSection =
    generatePDFfromParsed (sectionP superSectionT empty) sectionToText
  where
    sectionToText sec =
        let (latexSection, gs) = runState (toLaTeXM sec) initialGlobalState
         in renderLaTeX (labelToRef gs) (staticDocumentFormat <> document latexSection)

-- -- | TODO: what to do with auxiliary files and multiple requests in case of asynchronicity
-- mkPDF :: FilePath -> IO (Either String BS.ByteString)
-- mkPDF filename = do
--     let
--         pdfCommand = "pdflatex -interaction=nonstopmode -halt-on-error " <> filename
--         workingDir = "./src/Language/Ltml/ToLaTeX/Auxiliary"

--     -- Run pdflatex and capture output
--     (exitCode, stdout, _) <-
--         readCreateProcessWithExitCode
--             (shell pdfCommand) {cwd = Just workingDir}
--             ""

--     case exitCode of
--         ExitSuccess -> do
--             putStrLn "PDF generated successfully."
--             pdf <- BS.readFile "output.pdf"
--             return (Right pdf)
--         ExitFailure _ -> do
--             putStrLn "LaTeX compilation failed."
--             return $ Left $ drop 3094 stdout -- omitting the preambel of the pdflatex output here.
--             -- could be different on another system and thus maybe revert later
