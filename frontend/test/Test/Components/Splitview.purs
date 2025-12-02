module Test.Components.Splitview
  ( resizeFromLeftTest
  , resizeFromRightTest
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Data.Number (abs)
import Effect.Exception (Error)
import FPO.Components.Splitview (State, resizeFromLeft, resizeFromRight)
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
  } :: State

resizeFromLeftTest :: Spec Unit
resizeFromLeftTest =
  describe "resizeFromLeft" do
    it "when dragging to the left then make sidebar smaller and editor bigger" do

      let startSidebarSize = 0.2
      let startEditorSize = 0.4
      let startPreviewSize = 0.4
      let mousePercentFromLeft = 0.12

      let
        { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromLeft
          defaultState
            { startSidebarRatio = startSidebarSize
            , startEditorRatio = startEditorSize
            , startPreviewRatio = startPreviewSize
            }
          mousePercentFromLeft

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

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft

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

        let
          { sidebarClosed } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft

        sidebarClosed `shouldEqual` true

    it
      "when dragging to the right so that the editor is still bigger than preview just resize editor"
      do

        let startSidebarSize = 0.4
        let startEditorSize = 0.4
        let startPreviewSize = 0.2
        let mousePercentFromLeft = 0.45

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft

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

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft

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

        let
          { previewClosed } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft

        previewClosed `shouldEqual` true

    it
      "when starting with very slim sidebar and dragging to the right more than 5%, open it"
      do

        let
          startSidebarSize = 0.014209591474245116 -- real number from the system after opening the sidebar
          startEditorSize = 0.6
          startPreviewSize = 0.4 - startSidebarSize
          mousePercentFromLeft = 0.4

        let
          { sidebarClosed, newSidebarRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromLeft

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

        let
          { sidebarClosed, newSidebarRatio } = resizeFromLeft
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              , sidebarShown = sidebarShown
              }
            mousePercentFromLeft

        sidebarClosed `shouldEqual` true
        newSidebarRatio `shouldBeNear` 0.0

resizeFromRightTest :: Spec Unit
resizeFromRightTest =
  describe "resizeFromRight" do
    it "when dragging to the right then make preview smaller and editor bigger" do

      let startSidebarSize = 0.2
      let startEditorSize = 0.4
      let startPreviewSize = 0.4
      let mousePercentFromRight = 0.12

      let
        { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromRight
          defaultState
            { startSidebarRatio = startSidebarSize
            , startEditorRatio = startEditorSize
            , startPreviewRatio = startPreviewSize
            }
          mousePercentFromRight

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

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromRight
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromRight

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

        let
          { previewClosed } = resizeFromRight
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromRight

        previewClosed `shouldEqual` true

    it
      "when dragging to the right so that the editor is still bigger than preview just resize editor"
      do

        let startSidebarSize = 0.2
        let startEditorSize = 0.4
        let startPreviewSize = 0.4
        let mousePercentFromRight = 0.45

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromRight
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromRight

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

        let
          { newSidebarRatio, newEditorRatio, newPreviewRatio } = resizeFromRight
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromRight

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

        let
          { sidebarClosed } = resizeFromRight
            defaultState
              { startSidebarRatio = startSidebarSize
              , startEditorRatio = startEditorSize
              , startPreviewRatio = startPreviewSize
              }
            mousePercentFromRight

        sidebarClosed `shouldEqual` true

