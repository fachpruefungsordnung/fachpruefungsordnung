{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML.Export
    ( -- * Build ZIP Archive
      renderZip
    ) where

import Clay (render)
import Codec.Archive.Zip
import Data.ByteString.Lazy (ByteString)
import Data.Text (unpack)
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
        , tocEntryWrapperFunc = const anchorLink -- ignore category
        , tocButtonWrapperFunc = pageLink relativeSectionsDir
        , exportLinkWrapper = exportLink relativeSectionsDir
        }

exportSectionReaderState :: ReaderState
exportSectionReaderState =
    exportReaderState
        { labelWrapperFunc =
            mainPageAnchorLink (disjointRelative relativeSectionsDir "index.html")
        , exportLinkWrapper = const mempty -- no export links in exported view
        }

-------------------------------------------------------------------------------

-- TODO: Maybe for instantly self hosting

-- | Exports WHOLE document structure as HTML pages to given directory path
exportDocument :: Flagged' DocumentContainer -> FilePath -> IO ()
exportDocument docCon path =
    let mainDir = path </> mainDirectoryName
        absCssFilePath = mainDir </> relativeCssFilePath
        absSectionsDir = mainDir </> relativeSectionsDir
        (body, css) = renderHtmlCssWith exportReaderState initGlobalState docCon
        -- TODO: Get real Doc Title
        mainHtml = addHtmlHeader ("Temp Title" :: String) relativeCssFilePath body
     in do
            createDirectoryIfMissing True path
            createDirectoryIfMissing True (takeDirectory absCssFilePath)
            createDirectoryIfMissing True absSectionsDir

            writeFile absCssFilePath (toStrict $ render css)
            renderToFile (mainDir </> "index.html") mainHtml

-- | Renders WHOLE document structure as HTML pages to zip archive (as 'ByteString');
--   Returns @Nothing@, if AST contains any parse errors.
renderZip :: Flagged' DocumentContainer -> IO (Maybe ByteString)
renderZip docCon =
    -- TODO: check if Label "errors" occured not only parse erros
    let relativeHomePath = disjointRelative relativeSectionsDir "index.html"
        mHtmlCssParts =
            renderHtmlCssExport
                relativeHomePath
                exportReaderState
                initGlobalState
                exportSectionReaderState
                docCon
     in maybe (return Nothing) (fmap Just . buildZip) mHtmlCssParts
  where
    buildZip (mainBody, css, sectionBodies, rawTitle) =
        let
            mainHtml = addHtmlHeader rawTitle relativeCssFilePath mainBody
            mainBS = renderBS mainHtml
            stylesheetBS = encodeUtf8 $ render css
            sectionRelativeCssPath = disjointRelative relativeSectionsDir relativeCssFilePath
            sectionPathBS =
                map
                    ( \(tocId, title, html) ->
                        ( relativeSectionsDir </> unpack tocId <> ".html"
                        , renderBS $ addHtmlHeader title sectionRelativeCssPath html
                        )
                    )
                    sectionBodies
            files =
                [ ("index.html", mainBS)
                , (relativeCssFilePath, stylesheetBS)
                ]
                    ++ sectionPathBS
         in
            do
                currentTime <- round <$> getPOSIXTime
                let entries = map (\(path, bs) -> toEntry path currentTime bs) files
                    archive = foldr addEntryToArchive emptyArchive entries
                return $ fromArchive archive
