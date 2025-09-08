{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Export (exportDocument) where

import Clay (render)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (runState)
import Data.Text.IO (writeFile)
import Language.Ltml.AST.DocumentContainer (DocumentContainer)
import Language.Ltml.Common (Flagged')
import Language.Ltml.HTML
import Language.Ltml.HTML.CSS.Util
import Language.Ltml.HTML.Common
import Language.Ltml.HTML.Util
import Lucid
import System.Directory
import System.FilePath
import Prelude hiding (writeFile)
import Data.Text.Lazy (toStrict)

-- ReaderState felder mit wrapperFunktionen für:
--  - <a> für Section Headings (Sprung zur Einzelansicht)

-- @
-- <path>/
--     <mainDirectoryName>/
--         <realtiveCssFilePath>/
--             style.css
--         index.html
--         <relativeSectionDir>/
--             section1.html
--             section2.html
--             ...
-- @

mainDirectoryName :: FilePath
mainDirectoryName = "doc"

-- | Path to main CSS file relative to the index.html
relativeCssFilePath :: FilePath
relativeCssFilePath = "css" </> "style.css"

-- | Directory which holds all subppages relative to the index.html
relativeSectionsDir :: FilePath
relativeSectionsDir = "sections"

-------------------------------------------------------------------------------

-- | Exports document structure as HTML pages to given directory path
exportDocument :: Flagged' DocumentContainer -> FilePath -> IO ()
exportDocument docCon path =
    let mainDir = path </> mainDirectoryName
        absCssFilePath = mainDir </> relativeCssFilePath
        absSectionsDir = mainDir </> relativeSectionsDir
        (body, css) = renderHtmlCss docCon
        -- TODO: Get real Doc Title
        mainHtml = addHtmlHeader "Temp Title" relativeCssFilePath body
     in do
            createDirectoryIfMissing True path
            createDirectoryIfMissing True (takeDirectory absCssFilePath)
            createDirectoryIfMissing True absSectionsDir

            writeFile absCssFilePath (toStrict $ render css)
            renderToFile (mainDir </> "index.html") mainHtml

        -- mapState (exportSingleSection absSectionsDir) initGlobalState nodeSections

-- | Render section with given initial state and creates .html file
-- in given directory; returns the final state
-- exportSingleSection
--     :: (ToHtmlM a) => FilePath -> GlobalState -> a -> IO GlobalState
-- exportSingleSection path globalState a =
--     let (delayedHtml, finalState) = runState (runReaderT (toHtmlM a) initReaderState) globalState
--         body = evalDelayed delayedHtml finalState
--         sectionID = show (currentSectionID globalState)
--      in do
--             renderToFile (path </> ("section_" ++ sectionID ++ ".html")) $
--                 addHtmlHeader
--                     ("Einzelansicht § " ++ sectionID)
--                     (".." </> relativeCssFilePath)
--                     body
--             return finalState
