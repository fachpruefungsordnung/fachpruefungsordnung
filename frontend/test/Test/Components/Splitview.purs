module Test.Components.Splitview
  ( resizeFromLeftTest
  , resizeFromRightTest
  , togglePreviewTest
  , handleWindowResizeTest
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Effect.Class.Console (logShow)
import Effect.Exception (Error)
import FPO.Components.Splitview
  ( State
  , handleWindowResize
  , resizeFromLeft
  , resizeFromRight
  , togglePreview
  )
import FPO.Dto.DocumentDto.TreeDto (RootTree(..))
import FPO.Translations.Translator (fromFpoTranslator, translator)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

shouldBeNear :: forall m. MonadThrow Error m => Number -> Number -> m Unit
shouldBeNear expected actual = (abs (expected - actual) < 0.0001) `shouldEqual` true

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

resizeFromLeftTest :: Spec Unit
resizeFromLeftTest =
  describe "resizeFromLeft" do
    it "when dragging to the left then make sidebar smaller and editor bigger" do

      let startSidebarSize = 0.2
      let startEditorSize = 0.4
      let startPreviewSize = 0.4
      let mousePercentFromLeft = 0.12
      let width = 100000.0

      let
        { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromLeft
          defaultState
            { startSidebarRatio = startSidebarSize
            , startEditorRatio = startEditorSize
            , startPreviewRatio = startPreviewSize
            }
          mousePercentFromLeft
          width

      newSidebarRatio `shouldBeNear` 0.12
      newEditorRatio `shouldBeNear` 0.48
      newPreviewRatio `shouldBeNear` 0.4

    it
      "when dragging to the left more than 5% close to the left side, then sidebar ratio is 0"
      do

        let startSidebarSize = 0.2
        let startEditorSize = 0.4
        let startPreviewSize = 0.4
        let mousePercentFromLeft = 0.04887
        let width = 100000.0

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft
            width

        newSidebarRatio `shouldBeNear` 0.0
        newEditorRatio `shouldBeNear` 0.6
        newPreviewRatio `shouldBeNear` 0.4

    it
      "when dragging to the left more than 5% close to the left side, then hide sidebar"
      do

        let startSidebarSize = 0.2
        let startEditorSize = 0.4
        let startPreviewSize = 0.4
        let mousePercentFromLeft = 0.04887
        let width = 100000.0

        let
          { sidebarClosed } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft
            width

        sidebarClosed `shouldEqual` true

    it
      "when dragging to the right so that the editor is still bigger than preview just resize editor"
      do

        let startSidebarSize = 0.4
        let startEditorSize = 0.4
        let startPreviewSize = 0.2
        let mousePercentFromLeft = 0.45
        let width = 100000.0

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft
            width

        newSidebarRatio `shouldBeNear` 0.45
        newEditorRatio `shouldBeNear` 0.35
        newPreviewRatio `shouldBeNear` 0.2

    it
      "when dragging to the right so that the editor is smaller bigger than preview make both equally small"
      do

        let startSidebarSize = 0.2
        let startEditorSize = 0.4
        let startPreviewSize = 0.4
        let mousePercentFromLeft = 0.5
        let width = 100000.0

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft
            width

        newSidebarRatio `shouldBeNear` 0.5
        newEditorRatio `shouldBeNear` 0.25
        newPreviewRatio `shouldBeNear` 0.25

    it
      "when dragging to the right so that the preview is smaller than 10%, hide it"
      do

        let startSidebarSize = 0.2
        let startEditorSize = 0.4
        let startPreviewSize = 0.4
        let mousePercentFromLeft = 0.85
        let width = 100000.0

        let
          { previewClosed } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft
            width

        previewClosed `shouldEqual` true

    it
      "when starting with very slim sidebar and dragging to the right more than 5%, open it"
      do

        let
          startSidebarSize = 0.014209591474245116 -- real number from the system after opening the sidebar
          startEditorSize = 0.6
          startPreviewSize = 0.4 - startSidebarSize
          mousePercentFromLeft = 0.4
          width = 100000.0

        let
          { sidebarClosed, newSidebarRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft
            width

        sidebarClosed `shouldEqual` false
        newSidebarRatio `shouldBeNear` 0.4

    it
      "when starting with very slim sidebar and dragging to the right less than 5%, still close it"
      do

        let
          startSidebarSize = 0.014209591474245116 -- real number from the system after opening the sidebar
          startEditorSize = 0.6
          startPreviewSize = 0.4 - startSidebarSize
          mousePercentFromLeft = 0.04
          sidebarShown = false
          width = 100000.0

        let
          { sidebarClosed, newSidebarRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              , sidebarShown = sidebarShown
              }
            mousePercentFromLeft
            width

        sidebarClosed `shouldEqual` true
        newSidebarRatio `shouldBeNear` 0.0

    it "when dragging so far to the right that preview is smaller than 0.05, close it"
      do

        let startSidebarSize = 0.2
        let startEditorSize = 0.4
        let startPreviewSize = 0.4
        let mousePercentFromLeft = 0.91
        let width = 100000.0

        let
          { previewClosed } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft
            width

        previewClosed `shouldEqual` true

    it "consider resizer width for calculations"
      do

        let startSidebarSize = 0.2
        let startEditorSize = 0.4
        let startPreviewSize = 0.4
        let mousePercentFromLeft = 0.5
        let width = 160.0

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            (mousePercentFromLeft)
            width

        newSidebarRatio `shouldBeNear` 0.46666666
        newEditorRatio `shouldBeNear` 0.216666666
        newPreviewRatio `shouldBeNear` 0.21666666

resizeFromRightTest :: Spec Unit
resizeFromRightTest =
  describe "resizeFromRight" do
    it "when dragging to the right then make preview smaller and editor bigger" do

      let startSidebarSize = 0.2
      let startEditorSize = 0.4
      let startPreviewSize = 0.4
      let mousePercentFromRight = 0.12
      let width = 100000.0

      let
        { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromRight
          defaultState
            { startSidebarRatio = startSidebarSize
            , startEditorRatio = startEditorSize
            , startPreviewRatio = startPreviewSize
            }
          mousePercentFromRight
          width

      newSidebarRatio `shouldBeNear` 0.2
      newEditorRatio `shouldBeNear` 0.68
      newPreviewRatio `shouldBeNear` 0.12

    it
      "when dragging to the right more than 5% close to the left side, then set preview ratio to 0"
      do

        let startSidebarSize = 0.2
        let startEditorSize = 0.4
        let startPreviewSize = 0.4
        let mousePercentFromRight = 0.04887
        let width = 100000.0

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromRight
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromRight
            width

        newSidebarRatio `shouldBeNear` 0.2
        newEditorRatio `shouldBeNear` 0.8
        newPreviewRatio `shouldBeNear` 0.0

    it
      "when dragging to the right more than 5% close to the left side, then hide preview"
      do

        let startSidebarSize = 0.2
        let startEditorSize = 0.4
        let startPreviewSize = 0.4
        let mousePercentFromRight = 0.04887
        let width = 100000.0

        let
          { previewClosed } = resizeFromRight
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromRight
            width

        previewClosed `shouldEqual` true

    it
      "when dragging to the right so that the editor is still bigger than preview just resize editor"
      do

        let startSidebarSize = 0.2
        let startEditorSize = 0.4
        let startPreviewSize = 0.4
        let mousePercentFromRight = 0.45
        let width = 100000.0

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromRight
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromRight
            width

        newSidebarRatio `shouldBeNear` 0.2
        newEditorRatio `shouldBeNear` 0.35
        newPreviewRatio `shouldBeNear` 0.45

    it
      "when dragging to the right so that the editor is smaller bigger than preview make both equally small"
      do

        let startSidebarSize = 0.4
        let startEditorSize = 0.4
        let startPreviewSize = 0.2
        let mousePercentFromRight = 0.5
        let width = 100000.0

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromRight
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromRight
            width

        newSidebarRatio `shouldBeNear` 0.25
        newEditorRatio `shouldBeNear` 0.25
        newPreviewRatio `shouldBeNear` 0.5

    it
      "when dragging to the left so that the sidebar is smaller than 10%, hide it"
      do

        let startSidebarSize = 0.4
        let startEditorSize = 0.4
        let startPreviewSize = 0.2
        let mousePercentFromRight = 0.85
        let width = 100000.0

        let
          { sidebarClosed } = resizeFromRight
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromRight
            width

        sidebarClosed `shouldEqual` true

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
        oldWidth = 200.0
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