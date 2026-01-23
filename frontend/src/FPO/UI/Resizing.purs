module FPO.UI.Resizing
  ( ResizeState
  , resizeFromLeft
  , resizeFromRight
  , togglePreview
  , toggleSidebar
  , resizersTotalWidth
  ) where

import Prelude

-- Width of a single resizer in pixels
resizerWidth :: Number
resizerWidth = 8.0

-- The total width taken up by resizers (2 resizers × resizerWidth each)
resizersTotalWidth :: Number
resizersTotalWidth = 2.0 * resizerWidth

-- Minimum space required when toggling panels
minEditorSpaceForToggle :: Number
minEditorSpaceForToggle = 0.2

minSidebarSpaceForToggle :: Number
minSidebarSpaceForToggle = 0.1

minPreviewSpaceForToggle :: Number
minPreviewSpaceForToggle = 0.15

-- Minimum panel sizes to prevent closing during resize
minPanelSize :: Number
minPanelSize = 0.1

minSidebarSizeBeforeClosing :: Number
minSidebarSizeBeforeClosing = 0.05

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
  , commentClosed :: Boolean
  , tocClosed :: Boolean
  , commentOverviewClosed :: Boolean
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
    contentWidth = resizeState.windowWidth - resizersTotalWidth
    previewWidth = contentWidth * resizeState.previewRatio
    editorWidth = contentWidth * resizeState.editorRatio
    sidebarWidth = contentWidth * resizeState.sidebarRatio
    openSidebarThreshold = 0.05
    closeSidebarThreshold = 0.05
    sidebarRatio = mousePxFromLeft / contentWidth
  in
    if resizeState.sidebarClosed then
      if mousePercentFromLeft < openSidebarThreshold then
        resizeState
      else if
        mousePxFromLeft <= sidebarWidth
          -- resizing to the left but not close enough to hide sidebar
          || (mousePxFromLeft >= sidebarWidth) &&
            ( editorWidth - (mousePxFromLeft - sidebarWidth - resizerWidth) >=
                previewWidth
            )
      -- OR resizing to the left but preview still bigger than editor
      then
        resizeState
          { sidebarRatio = sidebarRatio
          , editorRatio = sidebarAndEditor - sidebarRatio
          , sidebarClosed = false
          }
      -- resizing to the right and preview bigger than editor
      else
        let
          newEditorAndPreview = (1.0 - sidebarRatio) / 2.0
        in
          if newEditorAndPreview >= minPanelSize then
            resizeState
              { sidebarRatio = sidebarRatio
              , editorRatio = newEditorAndPreview
              , previewRatio = newEditorAndPreview
              , sidebarClosed = false
              }
          else
            resizeState
              { sidebarRatio = sidebarRatio
              , editorRatio = 1.0 - sidebarRatio
              , previewRatio = 0.0
              , previewClosed = true
              , sidebarClosed = false
              }
    -- Close sidebar when dragging close enough to left edge (< 5%)
    else if mousePercentFromLeft < closeSidebarThreshold then
      resizeState
        { sidebarRatio = 0.0
        , editorRatio = sidebarAndEditor
        , sidebarClosed = true
        -- Save current sidebar ratio before closing (for later restoration)
        , lastExpandedSidebarRatio = resizeState.sidebarRatio
        }
    else if
      mousePxFromLeft <= sidebarWidth
        -- resizing to the left but not close enough to hide sidebar
        || (mousePxFromLeft >= sidebarWidth) &&
          ( editorWidth - (mousePxFromLeft - sidebarWidth - resizerWidth) >=
              previewWidth
          )
    -- OR resizing to the left but preview still bigger than editor
    then
      resizeState
        { sidebarRatio = sidebarRatio
        , editorRatio = sidebarAndEditor - sidebarRatio
        }
    -- resizing to the right and preview bigger than editor
    else
      let
        newEditorAndPreview = (1.0 - sidebarRatio) / 2.0
      in
        if newEditorAndPreview >= minPanelSize then
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
    contentWidth = resizeState.windowWidth - resizersTotalWidth
    sidebarWidth = contentWidth * resizeState.sidebarRatio
    editorWidth = contentWidth * resizeState.editorRatio
    previewWidth = contentWidth * resizeState.previewRatio
    mousePercentFromRight = mousePxFromRight / resizeState.windowWidth
    openPreviewThreshold = 0.05
    closePreviewThreshold = 0.10
    minPreviewRatio = 0.10
    previewRatio = max minPreviewRatio (mousePxFromRight / contentWidth)
  in
    if resizeState.previewClosed then
      if mousePercentFromRight < openPreviewThreshold then
        resizeState
      else if
        mousePxFromRight <= previewWidth
          -- resizing to the right but not close enough to hide preview
          || (mousePxFromRight >= previewWidth) &&
            ( editorWidth - (mousePxFromRight - previewWidth - resizerWidth) >=
                sidebarWidth
            )
      -- OR resizing to the right but sidebar still bigger than editor
      then
        resizeState
          { previewRatio = previewRatio
          , editorRatio = previewAndEditor - previewRatio
          , previewClosed = false
          }
      -- Hide sidebar when dragging close to left edge (5%)
      else
        let
          newEditorAndSidebar = (1.0 - previewRatio) / 2.0
        in
          -- resizing to the left and sidebar bigger than editor - make them equal
          if newEditorAndSidebar > minSidebarSizeBeforeClosing then
            resizeState
              { previewRatio = previewRatio
              , editorRatio = newEditorAndSidebar
              , sidebarRatio = newEditorAndSidebar
              , previewClosed = false
              }
          else
            resizeState
              { sidebarRatio = 0.0
              , previewRatio = previewRatio
              , editorRatio = 1.0 - previewRatio
              , sidebarClosed = true
              -- Save current sidebar ratio before closing (for later restoration)
              , lastExpandedSidebarRatio = resizeState.sidebarRatio
              , previewClosed = false
              }
    -- Hide preview when dragging close to right edge (< 10%)
    else if mousePercentFromRight < closePreviewThreshold then
      resizeState
        { previewRatio = 0.0
        , editorRatio = previewAndEditor
        , previewClosed = true
        -- Save current preview ratio before closing (for later restoration)
        , lastExpandedPreviewRatio = resizeState.previewRatio
        }
    else if
      mousePxFromRight <= previewWidth
        -- resizing to the right but not close enough to hide preview
        || (mousePxFromRight >= previewWidth) &&
          ( editorWidth - (mousePxFromRight - previewWidth - resizerWidth) >=
              sidebarWidth
          )
    -- OR resizing to the right but sidebar still bigger than editor
    then
      resizeState
        { previewRatio = previewRatio
        , editorRatio = previewAndEditor - previewRatio
        }
    -- Hide sidebar when dragging close to left edge (5%)
    else
      let
        newEditorAndSidebar = (1.0 - previewRatio) / 2.0
      in
        -- resizing to the left and sidebar bigger than editor - make them equal
        if newEditorAndSidebar > minSidebarSizeBeforeClosing then
          resizeState
            { previewRatio = previewRatio
            , editorRatio = newEditorAndSidebar
            , sidebarRatio = newEditorAndSidebar
            }
        else
          resizeState
            { sidebarRatio = 0.0
            , previewRatio = previewRatio
            , editorRatio = 1.0 - previewRatio
            , sidebarClosed = true
            -- Save current sidebar ratio before closing (for later restoration)
            , lastExpandedSidebarRatio = resizeState.sidebarRatio
            }

togglePreview :: ResizeState -> ResizeState
togglePreview resizeState =
  if resizeState.previewClosed then
    -- if there is enough space to shrink the editor, do it
    if
      resizeState.lastExpandedPreviewRatio <
        (resizeState.editorRatio - minEditorSpaceForToggle) then
      resizeState
        { previewClosed = false
        , previewRatio = resizeState.lastExpandedPreviewRatio
        , editorRatio = resizeState.editorRatio - resizeState.lastExpandedPreviewRatio
        }
    -- if not, try to shrink the sidebar
    else if
      resizeState.lastExpandedPreviewRatio <
        (resizeState.sidebarRatio - minSidebarSpaceForToggle) then
      resizeState
        { previewClosed = false
        , previewRatio = resizeState.lastExpandedPreviewRatio
        , sidebarRatio = resizeState.sidebarRatio -
            resizeState.lastExpandedPreviewRatio
        }
    else
      -- if there is not space, evenly distribute the space 
      -- from the sum of the not fitting windows (see test for real numbers)
      let
        sumOfAllRatios = resizeState.lastExpandedPreviewRatio
          + resizeState.editorRatio
          + resizeState.sidebarRatio
      in
        resizeState
          { previewClosed = false
          , sidebarRatio = resizeState.sidebarRatio / sumOfAllRatios
          , editorRatio = resizeState.editorRatio / sumOfAllRatios
          , previewRatio = resizeState.lastExpandedPreviewRatio / sumOfAllRatios
          }
  else
    -- Close preview and expand editor
    resizeState
      { previewClosed = true
      , previewRatio = 0.0
      -- Save current preview ratio before closing (for later restoration)
      , lastExpandedPreviewRatio = resizeState.previewRatio
      , editorRatio = resizeState.previewRatio + resizeState.editorRatio
      }

toggleSidebar :: ResizeState -> ResizeState
toggleSidebar resizeState =
  if resizeState.sidebarClosed then
    -- if there is enough space to shrink the editor, do it
    if
      resizeState.lastExpandedSidebarRatio <
        (resizeState.editorRatio - minEditorSpaceForToggle) then
      resizeState
        { sidebarClosed = false
        , sidebarRatio = resizeState.lastExpandedSidebarRatio
        , editorRatio = resizeState.editorRatio - resizeState.lastExpandedSidebarRatio
        }
    -- if not, try to shrink the preview
    else if
      resizeState.lastExpandedSidebarRatio <
        (resizeState.previewRatio - minPreviewSpaceForToggle) then
      resizeState
        { sidebarClosed = false
        , sidebarRatio = resizeState.lastExpandedSidebarRatio
        , previewRatio = resizeState.previewRatio -
            resizeState.lastExpandedSidebarRatio
        }
    else
      -- if there is not space, evenly distribute the space 
      -- from the sum of the not fitting windows (see test for real numbers)
      let
        sumOfAllRatios = resizeState.lastExpandedSidebarRatio
          + resizeState.editorRatio
          + resizeState.previewRatio
      in
        resizeState
          { sidebarClosed = false
          , previewRatio = resizeState.previewRatio / sumOfAllRatios
          , editorRatio = resizeState.editorRatio / sumOfAllRatios
          , sidebarRatio = resizeState.lastExpandedSidebarRatio / sumOfAllRatios
          }
  else
    -- Close sidebar and expand editor
    resizeState
      { sidebarClosed = true
      , sidebarRatio = 0.0
      -- Save current sidebar ratio before closing (for later restoration)
      , lastExpandedSidebarRatio = resizeState.sidebarRatio
      , editorRatio = resizeState.sidebarRatio + resizeState.editorRatio
      }
