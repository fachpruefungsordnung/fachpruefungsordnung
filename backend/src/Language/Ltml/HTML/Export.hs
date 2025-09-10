{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Export (exportDocument, renderZip) where

import Clay (render)
import Codec.Archive.Zip
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Data.Text (pack, unpack)
import Data.Text.IO (writeFile)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Language.Ltml.AST.DocumentContainer (DocumentContainer)
import Language.Ltml.Common (Flagged')
import Language.Ltml.HTML
import Language.Ltml.HTML.CSS.Util
import Language.Ltml.HTML.Common
import Language.Ltml.HTML.Util
import Lucid
import System.Directory
import System.FilePath.Posix
import Prelude hiding (writeFile)

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

exportReaderState :: ReaderState
exportReaderState =
    initReaderState
        { shouldRender = True
        , labelWrapperFunc = anchorLink
        , footnoteWrapperFunc = anchorLink
        , tocEntryWrapperFunc = anchorLink
        , tocButtonWrapperFunc = pageLink (pack relativeSectionsDir)
        }

-------------------------------------------------------------------------------

-- | Exports WHOLE document structure as HTML pages to given directory path
exportDocument :: Flagged' DocumentContainer -> FilePath -> IO ()
exportDocument docCon path =
    let mainDir = path </> mainDirectoryName
        absCssFilePath = mainDir </> relativeCssFilePath
        absSectionsDir = mainDir </> relativeSectionsDir
        (body, css) = renderHtmlCssWith exportReaderState initGlobalState docCon
        -- TODO: Get real Doc Title
        mainHtml = addHtmlHeader "Temp Title" relativeCssFilePath body
     in do
            createDirectoryIfMissing True path
            createDirectoryIfMissing True (takeDirectory absCssFilePath)
            createDirectoryIfMissing True absSectionsDir

            writeFile absCssFilePath (toStrict $ render css)
            renderToFile (mainDir </> "index.html") mainHtml

-- | Renders WHOLE document structure as HTML pages to zip archive (as 'ByteString')
renderZip :: Flagged' DocumentContainer -> IO ByteString
renderZip docCon =
    let (mainBody, css, sectionBodies) = renderHtmlCssExport exportReaderState initGlobalState docCon
        -- TODO: Get real Doc Title
        mainHtml = addHtmlHeader "Temp Title" relativeCssFilePath mainBody
        mainBS = renderBS mainHtml
        stylesheetBS = encodeUtf8 $ render css
        sectionRelativeCssPath = disjointRelative relativeSectionsDir relativeCssFilePath
        -- TODO get real Section Title
        sectionPathBS =
            map
                ( bimap
                    (\tocId -> relativeSectionsDir </> unpack tocId <> ".html")
                    (renderBS . addHtmlHeader "Section Title" sectionRelativeCssPath)
                )
                sectionBodies
        files =
            [ ("index.html", mainBS)
            , (relativeCssFilePath, stylesheetBS)
            ]
                ++ sectionPathBS
     in do
            currentTime <- round <$> getPOSIXTime
            let entries = map (\(path, bs) -> toEntry path currentTime bs) files
                archive = foldr addEntryToArchive emptyArchive entries
            return $ fromArchive archive

----------------------------------------------------------------------------------------------

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
