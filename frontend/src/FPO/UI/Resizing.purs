module FPO.UI.Resizing
  ( ResizeState
  , resizeFromLeft
  , resizeFromRight
  , togglePreview
  , toggleSidebar
  ) where

import Prelude

type ResizeState =
  { windowWidth :: Number
  -- All the ratios here are ratios of the content width (i.e., total width minus resizers)
  -- so all of the ratios should sum to 1.0
  , sidebarRatio :: Number
  , previewRatio :: Number
  , editorRatio :: Number
  , lastExpandedSidebarRatio :: Number
  , lastExpandedPreviewRatio :: Number
  , sidebarClosed :: Boolean
  , previewClosed :: Boolean
  }

resizeFromLeft
  :: ResizeState
  -> Number
  -> ResizeState
resizeFromLeft
  resizeState
  mousePxFromLeft =
  let
    sidebarAndEditor = resizeState.sidebarRatio + resizeState.editorRatio
    mousePercentFromLeft = mousePxFromLeft / resizeState.windowWidth
    contentWidth = resizeState.windowWidth - 16.0
    previewWidth = contentWidth * resizeState.previewRatio
    editorWidth = contentWidth * resizeState.editorRatio
    sidebarWidth = contentWidth * resizeState.sidebarRatio
  in
    -- close enough to hide sidebar
    if mousePercentFromLeft <= 0.05 then
      resizeState
        { sidebarRatio = 0.0
        , editorRatio = sidebarAndEditor
        , sidebarClosed = true
        }
    else if
      mousePxFromLeft <= sidebarWidth
        -- resizing to the left but not close enough to hide sidebar
        || (mousePxFromLeft >= sidebarWidth) &&
          (editorWidth - (mousePxFromLeft - sidebarWidth - 8.0) >= previewWidth)
    -- OR resizing to the left but preview still bigger than editor
    then
      let
        sidebarRatio = mousePxFromLeft / contentWidth
      in
        resizeState
          { sidebarRatio = sidebarRatio
          , editorRatio = sidebarAndEditor - sidebarRatio
          }
    -- resizing to the right and preview bigger than editor
    else
      let
        sidebarRatio = mousePxFromLeft / contentWidth
        newEditorAndPreview = (1.0 - sidebarRatio) / 2.0
      in
        if newEditorAndPreview >= 0.1 then
          resizeState
            { sidebarRatio = sidebarRatio
            , editorRatio = newEditorAndPreview
            , previewRatio = newEditorAndPreview
            }
        else
          resizeState
            { sidebarRatio = sidebarRatio
            , editorRatio = 1.0 - sidebarRatio
            , previewRatio = 0.0
            , previewClosed = true
            }

resizeFromRight
  :: ResizeState
  -> Number
  -> ResizeState
resizeFromRight
  resizeState
  mousePxFromRight =
  let
    previewAndEditor = resizeState.previewRatio + resizeState.editorRatio
    contentWidth = resizeState.windowWidth - 16.0
    sidebarWidth = contentWidth * resizeState.sidebarRatio
    editorWidth = contentWidth * resizeState.editorRatio
    previewWidth = contentWidth * resizeState.previewRatio
    -- Calculate position from left for sidebar closing logic
    mousePercentFromRight = mousePxFromRight / resizeState.windowWidth
  in
    -- Hide preview when dragging close to right edge (10%)
    if mousePercentFromRight <= 0.10 then
      resizeState
        { previewRatio = 0.0
        , editorRatio = previewAndEditor
        , previewClosed = true
        , lastExpandedPreviewRatio = resizeState.previewRatio
        }
    else if
      mousePxFromRight <= previewWidth
        -- resizing to the right but not close enough to hide preview
        || (mousePxFromRight >= previewWidth) &&
          (editorWidth - (mousePxFromRight - previewWidth - 8.0) >= sidebarWidth)
    -- OR resizing to the right but sidebar still bigger than editor
    then
      let
        previewRatio = mousePxFromRight / contentWidth
      in
        resizeState
          { previewRatio = previewRatio
          , editorRatio = previewAndEditor - previewRatio
          }
    -- Hide sidebar when dragging close to left edge (5%)
    else
      let
        previewRatio = mousePxFromRight / contentWidth
        newEditorAndSidebar = (1.0 - previewRatio) / 2.0
      in
        -- resizing to the left and sidebar bigger than editor - make them equal
        if newEditorAndSidebar > 0.05 then
          resizeState
            { previewRatio = previewRatio
            , editorRatio = newEditorAndSidebar
            , sidebarRatio = newEditorAndSidebar
            }
        else
          resizeState
            { sidebarRatio = 0.0
            , editorRatio = resizeState.sidebarRatio + resizeState.editorRatio
            , sidebarClosed = true
            , lastExpandedSidebarRatio = resizeState.sidebarRatio
            }

togglePreview :: ResizeState -> ResizeState
togglePreview resizeState =
  if resizeState.previewClosed then
    resizeState
      { previewClosed = false
      , previewRatio = resizeState.lastExpandedPreviewRatio
      , editorRatio = resizeState.editorRatio - resizeState.lastExpandedPreviewRatio
      }
  else
    resizeState
      { previewClosed = true
      , previewRatio = 0.0
      , lastExpandedPreviewRatio = resizeState.previewRatio
      , editorRatio = resizeState.previewRatio + resizeState.editorRatio
      }

toggleSidebar :: ResizeState -> ResizeState
toggleSidebar resizeState =
  if resizeState.sidebarClosed then
    resizeState
      { sidebarClosed = false
      , sidebarRatio = resizeState.lastExpandedSidebarRatio
      , editorRatio = resizeState.editorRatio - resizeState.lastExpandedSidebarRatio
      }
  else
    resizeState
      { sidebarClosed = true
      , sidebarRatio = 0.0
      , lastExpandedSidebarRatio = resizeState.sidebarRatio
      , editorRatio = resizeState.sidebarRatio + resizeState.editorRatio
      }
