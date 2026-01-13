module Test.Components.Splitview
  ( togglePreviewTest
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import FPO.Components.Splitview
  ( State
  , togglePreview
  )
import FPO.Dto.DocumentDto.TreeDto (RootTree(..))
import FPO.Translations.Translator (fromFpoTranslator, translator)
import FPO.UI.Resizing (ResizeState)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (shouldBeNear)

defaultResizeState =
  { windowWidth: 116.0 -- easy number for percent calculations, because content width - resizers is 100
  , sidebarRatio: 0.2
  , editorRatio: 0.4
  , previewRatio: 0.4
  , lastExpandedPreviewRatio: 0.4
  , lastExpandedSidebarRatio: 0.2
  , sidebarClosed: false
  , previewClosed: false
  } :: ResizeState

defaultState =
  { docID: 0
  , translator: fromFpoTranslator translator
  , mDragTarget: Nothing
  , sidebarRatio: 0.2
  , previewRatio: 0.4
  , editorRatio: 0.4
  , lastExpandedSidebarRatio: 0.2
  , lastExpandedPreviewRatio: 0.4
  , resizeState: defaultResizeState
  , mStartResizeState: Nothing
  , renderedHtml: Nothing
  , testDownload: ""
  , tocEntries: Empty
  , versionMapping: Empty
  , mTimeFormatter: Nothing
  , sidebarShown: true
  , tocShown: true
  , commentOverviewShown: false
  , commentShown: false
  , previewShown: true
  , mSelectedTocEntry: Nothing
  , dirtyVersion: false
  , modalData: Nothing
  , upToDateVersion: Nothing
  , pendingUpdateElementID: Nothing
  , mListener: Nothing
  , mResizeObserver: Nothing
  , mResizeSubscriptionId: Nothing
  } :: State

togglePreviewTest :: Spec Unit
togglePreviewTest =
  describe "togglePreviewTest" do
    it "when closing preview editor width gets bigger by preview size" do
      let
        resizerRatio = 16.0 / 1000.0
        sidebarRatio = 0.2 - (resizerRatio / 3.0)
        oldEditorRatio = 0.4 - (resizerRatio / 3.0)
        previewRatio = 0.4 - (resizerRatio / 3.0)

        { editorRatio } = togglePreview 1000.0
          defaultState
            { sidebarRatio = sidebarRatio
            , editorRatio = oldEditorRatio
            , previewRatio = previewRatio
            , previewShown = true
            }
      editorRatio `shouldBeNear` (1.0 - sidebarRatio - resizerRatio)

    it "when closing preview the flag is false" do
      let
        { previewShown } = togglePreview 1000.0
          defaultState
            { previewShown = true }
      previewShown `shouldEqual` false

    it "when opening preview the flag is true" do
      let
        { previewShown } = togglePreview 1000.0
          defaultState
            { previewShown = false }
      previewShown `shouldEqual` true

    it "saves last expanded preview ratio when closing preview" do
      let
        startPreviewSize = 0.2
        { lastExpandedPreviewRatio } = togglePreview 1000.0
          defaultState
            { previewRatio = startPreviewSize
            , previewShown = true
            }
      lastExpandedPreviewRatio `shouldBeNear` startPreviewSize

    it "restores last expanded preview ratio when opening preview" do
      let
        startLastExpandedPreviewSize = 0.3
        { previewRatio } = togglePreview 1000.0
          defaultState
            { lastExpandedPreviewRatio = startLastExpandedPreviewSize
            , previewShown = false
            }
      previewRatio `shouldBeNear` startLastExpandedPreviewSize
