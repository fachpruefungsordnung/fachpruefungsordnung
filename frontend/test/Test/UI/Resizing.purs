module Test.UI.Resizing
  ( resizeFromLeftTest
  , resizeFromRightTest
  , togglePreviewTest
  ) where

import Prelude

import FPO.UI.Resizing (ResizeState, resizeFromLeft, resizeFromRight, togglePreview)
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

resizeFromLeftTest :: Spec Unit
resizeFromLeftTest =
  describe "resizeFromLeft" do
    it "makes the sidebar smaller when dragging to the left" do
      let mousePxFromLeft = 12.0
      let { sidebarRatio } = resizeFromLeft defaultResizeState mousePxFromLeft

      sidebarRatio `shouldBeNear` 0.12

    it "makes sidebar ratio 0 when dragging to the left closer than 5%" do
      let mousePxFromLeft = 4.887

      let { sidebarRatio } = resizeFromLeft defaultResizeState mousePxFromLeft
      sidebarRatio `shouldBeNear` 0.0

    it "hides sidebar, when dragging to the left more than 5% close to the left side"
      do
        let mousePxFromLeft = 4.887
        let { sidebarClosed } = resizeFromLeft defaultResizeState mousePxFromLeft

        sidebarClosed `shouldEqual` true

    it
      "sets last expanded sidebar ratio, when dragging to the left more than 5% close to the left side"
      do
        let mousePxFromLeft = 4.887
        let
          { lastExpandedSidebarRatio } = resizeFromLeft defaultResizeState
            mousePxFromLeft

        lastExpandedSidebarRatio `shouldBeNear` 0.2

    it "makes the editor bigger when dragging to the left" do
      let mousePxFromLeft = 12.0
      let { editorRatio } = resizeFromLeft defaultResizeState mousePxFromLeft

      editorRatio `shouldBeNear` 0.48

    it
      "only makes the editor smaller when dragging to the right so that the editor is still bigger than preview"
      do
        let mousePxFromLeft = 35.0
        let
          { editorRatio } = resizeFromLeft
            ( defaultResizeState
                { sidebarRatio = 0.3, editorRatio = 0.4, previewRatio = 0.3 }
            )
            mousePxFromLeft

        editorRatio `shouldBeNear` 0.35

    it
      "makes preview and editor equally small when dragging to the right so that the editor is smaller than preview"
      do
        let mousePxFromLeft = 50.0
        let
          { editorRatio, previewRatio } = resizeFromLeft defaultResizeState
            mousePxFromLeft

        editorRatio `shouldBeNear` 0.25
        previewRatio `shouldBeNear` 0.25

    it
      "hides the preview, when dragging to the right more than 10% close to the right side"
      do
        let mousePxFromLeft = 85.0
        let { previewClosed } = resizeFromLeft defaultResizeState mousePxFromLeft

        previewClosed `shouldEqual` true

    it
      "sets the preview width to 0, when dragging to the right more than 10% close to the right side"
      do
        let mousePxFromLeft = 85.0
        let { previewRatio } = resizeFromLeft defaultResizeState mousePxFromLeft

        previewRatio `shouldBeNear` 0.0

    it
      "sets the latest preview width to start preview width, when dragging to the right more than 10% close to the right side"
      do
        let mousePxFromLeft = 85.0
        let
          { lastExpandedPreviewRatio } = resizeFromLeft defaultResizeState
            mousePxFromLeft

        lastExpandedPreviewRatio `shouldBeNear` 0.4

resizeFromRightTest :: Spec Unit
resizeFromRightTest =
  describe "resizeFromRight" do
    it "makes the preview smaller when dragging to the right" do
      let mousePxFromRight = 12.0
      let { previewRatio } = resizeFromRight defaultResizeState mousePxFromRight

      previewRatio `shouldBeNear` 0.12

    it "makes preview ratio 0 when dragging to the right closer than 10%" do
      let mousePxFromRight = 9.887

      let { previewRatio } = resizeFromRight defaultResizeState mousePxFromRight
      previewRatio `shouldBeNear` 0.0

    it
      "hides preview, when dragging to the right more than 10% close to the right side"
      do
        let mousePxFromRight = 9.887
        let { previewClosed } = resizeFromRight defaultResizeState mousePxFromRight

        previewClosed `shouldEqual` true

    it
      "sets last expanded preview ratio, when dragging to the right more than 10% close to the right side"
      do
        let mousePxFromRight = 9.887
        let
          { lastExpandedPreviewRatio } = resizeFromRight defaultResizeState
            mousePxFromRight

        lastExpandedPreviewRatio `shouldBeNear` 0.4

    it "makes the editor bigger when dragging to the right" do
      let mousePxFromRight = 12.0
      let { editorRatio } = resizeFromRight defaultResizeState mousePxFromRight

      editorRatio `shouldBeNear` 0.68

    it
      "only makes the editor smaller when dragging to the left so that the editor is still bigger than sidebar"
      do
        let mousePxFromRight = 35.0
        let
          { editorRatio } = resizeFromRight
            ( defaultResizeState
                { sidebarRatio = 0.3, editorRatio = 0.4, previewRatio = 0.3 }
            )
            mousePxFromRight

        editorRatio `shouldBeNear` 0.35

    it
      "makes sidebar and editor equally small when dragging to the left so that the editor is smaller than sidebar"
      do
        let mousePxFromRight = 50.0
        let
          { editorRatio, sidebarRatio } = resizeFromRight
            ( defaultResizeState
                { sidebarRatio = 0.3, editorRatio = 0.4, previewRatio = 0.3 }
            )
            mousePxFromRight

        editorRatio `shouldBeNear` 0.25
        sidebarRatio `shouldBeNear` 0.25

    it
      "hides the sidebar, when dragging to the left more than 5% close to the left side"
      do
        let mousePxFromRight = 100.0
        let { sidebarClosed } = resizeFromRight defaultResizeState mousePxFromRight

        sidebarClosed `shouldEqual` true

    it
      "sets the sidebar width to 0, when dragging to the left more than 5% close to the left side"
      do
        let mousePxFromRight = 100.0
        let { sidebarRatio } = resizeFromRight defaultResizeState mousePxFromRight

        sidebarRatio `shouldBeNear` 0.0

    it
      "sets the latest sidebar width to start sidebar width, when dragging to the left more than 5% close to the left side"
      do
        let mousePxFromRight = 100.0
        let
          { lastExpandedSidebarRatio } = resizeFromRight defaultResizeState
            mousePxFromRight

        lastExpandedSidebarRatio `shouldBeNear` 0.2

togglePreviewTest :: Spec Unit
togglePreviewTest =
  describe "togglePreviewTest" do
    it "closes preview, when it was open" do
      let { previewClosed } = togglePreview defaultResizeState
      previewClosed `shouldEqual` true

    it "sets preview width to 0.0, when it was open" do
      let { previewRatio } = togglePreview defaultResizeState
      previewRatio `shouldEqual` 0.0

    it "sets lastExpandedPreviewRatio to previous previewRatio, when it was open" do
      let { lastExpandedPreviewRatio } = togglePreview defaultResizeState
      lastExpandedPreviewRatio `shouldEqual` defaultResizeState.previewRatio

    it "opens preview, when it was closed" do
      let
        { previewClosed } = togglePreview
          (defaultResizeState { previewClosed = true })
      previewClosed `shouldEqual` false

    it "sets preview width to lastExpnadedPreviewRatio, when it was closed" do
      let
        { previewRatio } = togglePreview
          ( defaultResizeState
              { previewClosed = true, lastExpandedPreviewRatio = 0.15 }
          )
      previewRatio `shouldEqual` 0.15
