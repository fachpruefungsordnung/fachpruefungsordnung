{-# LANGUAGE OverloadedStrings #-}

module Language.Ltml.HTML
    ( -- * Rendering HTML
      renderSectionHtmlCss
    , renderHtmlCss
    , renderHtmlCssWith
    , renderHtmlCssExport
    , renderHtmlCssBS

      -- * Rendering ToC Headings
    , renderTocList
    ) where

import Clay (Css)
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.ByteString.Lazy (ByteString)
import Data.DList (toList)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Language.Ltml.AST.DocumentContainer
    ( DocumentContainer (..)
    )
import Language.Ltml.AST.Footnote (Footnote (..))
import Language.Ltml.AST.Label (Label (..))
import Language.Ltml.AST.Section
import Language.Ltml.Common (Flagged')
import Language.Ltml.HTML.CSS (mainStylesheet)
import qualified Language.Ltml.HTML.CSS.Classes as Class
import Language.Ltml.HTML.CSS.Util
import Language.Ltml.HTML.Common
import Language.Ltml.HTML.ToHtmlM (toHtmlM)
import Lucid

-- | Render single @Node Section@ with given 'Footnote' Map to @Html ()@ and @Css@
renderSectionHtmlCss
    :: FormattedSection -> Map.Map Label Footnote -> (Html (), Css)
renderSectionHtmlCss section fnMap =
    -- \| Render with given footnote context
    let readerState = initReaderState {footnoteMap = fnMap}
        (delayedHtml, finalState) = runReaderState (toHtmlM section) readerState initGlobalState
     in (evalDelayed finalState delayedHtml, mainStylesheet (enumStyles finalState))

-- | Render @Flagged' DocumentContainer@ to @Html ()@ and @Css@
renderHtmlCss :: Flagged' DocumentContainer -> (Html (), Css)
renderHtmlCss = renderHtmlCssWith initReaderState initGlobalState

-- | Render a @Flagged' DocumentContainer@ to HTML and CSS with a given
--   initial 'ReaderState' and 'GlobalState'
renderHtmlCssWith
    :: ReaderState -> GlobalState -> Flagged' DocumentContainer -> (Html (), Css)
renderHtmlCssWith readerState globalState docContainer =
    -- \| Render with given footnote context
    let (delayedHtml, finalState) = runReaderState (toHtmlM docContainer) readerState globalState
     in (evalDelayed finalState delayedHtml, mainStylesheet (enumStyles finalState))

-- | Render a @Flagged' DocumentContainer@ with given states to main HTML, main CSS,
--   and list of exported sections. Fails, if any parse errors occur.
renderHtmlCssExport
    :: FilePath
    -- ^ Path from exported Sections to main HTML
    -> ReaderState
    -- ^ Used for document container
    -> GlobalState
    -> ReaderState
    -- ^ Used for exported sections
    -> Flagged' DocumentContainer
    -> Maybe
        ( Html ()
        , -- \^ HTML of whole Document
          Css
        , -- \^ Main Stylesheet
          [(Text, Text, Html ())]
        , -- \^ Exported sections Html with id and title
          Text
          -- \^ Raw textual title of the main document
        )
renderHtmlCssExport backPath readerState globalState exportReaderState docCon =
    -- \| Render with given footnote context
    let (delayedHtml, finalState) = runState (runReaderT (toHtmlM docCon) readerState) globalState
        -- \| Add footnote labes for "normal" (non-footnote) references
        mainHtml = evalDelayed finalState delayedHtml
        css = mainStylesheet (enumStyles finalState)
        mainDocTitleHtml = mainDocumentTitleHtml finalState
        rawMainDocTitle = evalDelayed finalState $ mainDocumentTitle finalState
        -- \| Second render for exported sections
        -- TODO: get rid of second render run (its only because of the different labelWrapperFuncs)
        (_, finalExportState) = runReaderState (toHtmlM docCon) exportReaderState globalState
        backButton =
            div_ $
                a_ [cssClass_ Class.ButtonLink, href_ $ pack backPath] (toHtml ("←" :: Text))
        sections =
            map
                ( \(htmlId, dTitle, dHtml) ->
                    ( htmlId
                    , evalDelayed finalExportState dTitle
                    , evalDelayed finalExportState
                        . ( fmap (pure backButton <> div_ <#> Class.Document)
                                . (mainDocTitleHtml <>)
                          )
                        $ dHtml
                    )
                )
                (exportSections finalExportState)
     in if hasErrors finalState
            then Nothing
            else Just (mainHtml, css, sections, rawMainDocTitle)

-- | Renders a @Flagged' DocumentContainer@ to HTML 'ByteString' with inlined CSS
renderHtmlCssBS :: Flagged' DocumentContainer -> ByteString
renderHtmlCssBS docCon =
    let (body, css) = renderHtmlCss docCon
     in renderBS $ addInlineCssHeader "Generated Document Preview" css body

-------------------------------------------------------------------------------

-- | Renders a global ToC (including appendices) as a list of
--   (@Maybe@ idText, @Result@ titleText). 
--   The @Result@ type signals if an error occured while
--   parsing the segment.
renderTocList
    :: Flagged' DocumentContainer -> [FrontendTocEntry]
renderTocList docContainer =
    -- \| Create global ToC inlcuding appendices
    let (_, finalState) = runReaderState (toHtmlM docContainer) initReaderState initGlobalState
     in toList $ frontendToC finalState

-------------------------------------------------------------------------------
