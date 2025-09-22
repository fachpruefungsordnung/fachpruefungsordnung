{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Ltml.ToLaTeX.PDFGenerator
    ( generatePDFFromSection -- deprecated, use 'generatePDF' instead
    , generatePDF
    , generateLaTeX
    --   generatePDFFromDocument
    ) where

import Control.Exception (Exception (displayException), SomeException, try)
import Control.Lens (view)
import Control.Monad.State (runState)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Language.Ltml.ToLaTeX.GlobalState
    ( initialGlobalState
    , labelToRef
    , preDocument
    )
import Language.Ltml.ToLaTeX.PreLaTeXType (document)
import Language.Ltml.ToLaTeX.Renderer (renderLaTeX)
import Language.Ltml.ToLaTeX.ToLaTeX (toLaTeX)
import Language.Ltml.ToLaTeX.ToPreLaTeXM (ToPreLaTeXM (toPreLaTeXM))
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
    ( CreateProcess (cwd, std_err, std_in, std_out)
    , StdStream (CreatePipe)
    , createProcess
    , proc
    , waitForProcess
    )

-------------------------------- Public -----------------------------------

generatePDF
    :: (ToPreLaTeXM a) => a -> IO (Either String BSL.ByteString)
generatePDF input = do
    res <- try $ compilePDF input
    case res of
        Left (e :: SomeException) -> return $ Left (displayException e)
        Right pdf -> return $ Right pdf

-------------------------------- Pipeline -----------------------------------

compilePDF
    :: (ToPreLaTeXM p) => p -> IO BSL.ByteString
compilePDF input =
    withSystemTempDirectory "latex-temp" $ \tmpDir -> do
        let texFile = tmpDir </> "input.tex"
            pdfFile = tmpDir </> "input.pdf"

        -- Generate LaTeX source from input
        let latexSource = generateLaTeX input

        -- Write LaTeX source
        BS.writeFile texFile (TE.encodeUtf8 latexSource)

        -- Compile with pdflatex
        (exitCode, stdout, _) <- runLatex texFile tmpDir

        case exitCode of
            ExitFailure _ -> failLatex stdout
            ExitSuccess -> BSL.readFile pdfFile

-------------------------------- Helpers -----------------------------------

generateLaTeX :: (ToPreLaTeXM a) => a -> Text
generateLaTeX input =
    let (res, gs) = runState (toPreLaTeXM input) initialGlobalState
     in renderLaTeX $
            toLaTeX
                (view labelToRef gs)
                (view preDocument gs <> document res)

runLatex :: FilePath -> FilePath -> IO (ExitCode, BS.ByteString, BS.ByteString)
runLatex texFile workDir = do
    (Just hin, Just hout, Just herr, ph) <-
        createProcess
            (proc "latexmk" ["-pdf", "-interaction=nonstopmode", "-halt-on-error", texFile])
                { cwd = Just workDir
                , std_in = CreatePipe
                , std_out = CreatePipe
                , std_err = CreatePipe
                }
    hClose hin
    out <- BS.hGetContents hout
    err <- BS.hGetContents herr
    exitCode <- waitForProcess ph
    return (exitCode, out, err)

failLatex :: BS.ByteString -> IO a
failLatex stdout =
    -- TODO: maybe drop preambel here
    fail (BS.unpack stdout)

-------------------------------- Deprecated -----------------------------------

generatePDFFromSection :: Text -> IO (Either String BSL.ByteString)
generatePDFFromSection = undefined

--     let NamedType _ _ sectionT' = sectionT
--         NamedType _ _ footnoteT' = footnoteT
--      in generatePDFfromParsed
--             (nSc *> runFootnoteWriterT (sectionP sectionT' eof) [footnoteT'])
--             sectionToText
--             (input <> "\n")
--   where
--     sectionToText (sec, labelmap) =
--         let (latexSection, gs) = runState (toPreLaTeXM sec) $ initialGlobalState & labelToFootNote .~ labelmap
--          in renderLaTeX $
--                 toLaTeX (view labelToRef gs) (view preDocument gs <> document latexSection)

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

-- withTempIn :: FilePath -> String -> (FilePath -> IO a) -> IO a
-- withTempIn parent template =
--     bracket
--         (createTempDirectory parent template)
--         removeDirectoryRecursive
