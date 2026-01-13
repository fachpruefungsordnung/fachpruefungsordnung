module Test.Components.Splitview
  ( togglePreviewTest
  , handleWindowResizeTest
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import FPO.Components.Splitview
  ( State
  , handleWindowResize
  , togglePreview
  )
import FPO.Dto.DocumentDto.TreeDto (RootTree(..))
import FPO.Translations.Translator (fromFpoTranslator, translator)
import FPO.UI.Resizing (ResizeState)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Util (shouldBeNear)

defaultResizeState =
  { windowWidth: 116 -- easy number for percent calculations, because content width - resizers is 100
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
  , startMouseRatio: 0.0
  , startSidebarRatio: 0.0
  , startPreviewRatio: 0.0
  , startEditorRatio: 0.0
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

handleWindowResizeTest :: Spec Unit
handleWindowResizeTest =
  describe "handleWindowResize" do
    it "adjusts ratios correctly when window is resized" do
      let
        oldWidth = 160.0
        newWidth = 320.0

        -- Resizer nimmt immer 16px ein
        oldResizerRatio = 16.0 / oldWidth -- 0.1 (10% vom alten Window)
        newResizerRatio = 16.0 / newWidth -- 0.05 (5% vom neuen Window)

        -- Content space im alten vs. neuen Fenster
        oldContentSpace = 1.0 - oldResizerRatio -- 0.9 (90%)
        newContentSpace = 1.0 - newResizerRatio -- 0.95 (95%)

        -- Initial ratios als WINDOW SPACE (nicht content space!)
        -- Diese müssen zusammen mit Resizer-Ratio = 1.0 ergeben
        startSidebarRatio = 0.27 -- 30% von 0.9 = 0.27 vom Window
        startEditorRatio = 0.36 -- 40% von 0.9 = 0.36 vom Window
        startPreviewRatio = 0.27 -- 30% von 0.9 = 0.27 vom Window
        -- Total: 0.27 + 0.36 + 0.27 = 0.9 (+ 0.1 Resizer = 1.0)

        -- Scale factor für Content-Bereiche
        scaleFactor = newContentSpace / oldContentSpace -- 0.95 / 0.9 = 1.0556

        -- Erwartete neue Window-Ratios
        expectedSidebarRatio = startSidebarRatio * scaleFactor -- ~0.285
        expectedEditorRatio = startEditorRatio * scaleFactor -- ~0.38  
        expectedPreviewRatio = startPreviewRatio * scaleFactor -- ~0.285

        result = handleWindowResize newWidth
          defaultState
            { sidebarRatio = startSidebarRatio
            , editorRatio = startEditorRatio
            , previewRatio = startPreviewRatio
            }

      result.sidebarRatio `shouldBeNear` expectedSidebarRatio
      result.editorRatio `shouldBeNear` expectedEditorRatio
      result.previewRatio `shouldBeNear` expectedPreviewRatio

      -- Verify total adds up correctly with new resizer ratio
      let
        totalRatio = result.sidebarRatio + result.editorRatio + result.previewRatio +
          newResizerRatio
      totalRatio `shouldBeNear` 1.0

    it "preserves proportional relationships" do
      let
        newWidth = 400.0

        startSidebarRatio = 0.2
        startEditorRatio = 0.6
        startPreviewRatio = 0.2

        result = handleWindowResize newWidth
          defaultState
            { sidebarRatio = startSidebarRatio
            , editorRatio = startEditorRatio
            , previewRatio = startPreviewRatio
            }

      -- The ratios between sections should be preserved
      let
        originalSidebarToEditor = startSidebarRatio / startEditorRatio -- 0.333...
        newSidebarToEditor = result.sidebarRatio / result.editorRatio

        originalEditorToPreview = startEditorRatio / startPreviewRatio -- 3.0
        newEditorToPreview = result.editorRatio / result.previewRatio

      newSidebarToEditor `shouldBeNear` originalSidebarToEditor
      newEditorToPreview `shouldBeNear` originalEditorToPreview