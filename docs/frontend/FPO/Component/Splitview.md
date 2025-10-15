# FPO.Components.Splitview

Splitview is the main component that is used in EditorPage. It is the parent component and has multiple child components, split into three separate views. Depending on the task, the child could be displayed either on the left, middle, or right side of the split view. The Splitview component is thus the coordinator for all the children. It either redirects data from one component to another (e.g. selecting an entry in TOC should notify the [editor](Editor.md) component by sending the corresponding ID), only gets data from its children (e.g. closing request from the Comment component), or only sends data to a child (e.g. sending a newly received tree from the backend to TOC). The middle view only has the main [editor](Editor.md) and is always visible. The other views can each only harbor one child component at a time and can also be closed by the user.

### Splitview has the following children:

- Left view:
  - TOC (Table of contents)
  - Comment
  - CommentOverview
- Middle view:
  - [Editor](Editor.md) (0 is main editor)
- Right view:
  - [Editor](Editor.md) (1 is compare editor)
  - Preview

## Important Interactions

### Request

Splitview is the component responsible for handling the requests regarding the document tree from and to the backend. In a GET request, the tree is stored in the state as `tocEntries` and then sent to TOC. The POST is only called by TOC to update the new structure of the tree to the backend.

#### Request uses the following Actions:

- GET
- POST

#### Request uses the following state labels:

- tocEntries
- versionMapping

### Resize

The Splitview gives the user the ability to resize the size of the three split views. It uses a DOM element referred to here as the resizer. The resizer visualizes the border of the middle view with either one of the other views. With mouse events, the user can drag and drop the resizer to resize the sizes, which are calculated in percentage. While dragging, the views cannot be smaller than a set size, which is 5%. While the middle and right views’ max size is dependent on the min sizes of the other views, the left view’s max size is capped at 20%.

#### Resize uses the following Actions:

- StartResize DragTarget MouseEvent
- StopResize MouseEvent
- HandleMouseMove MouseEvent

#### Resize uses the following state labels:

- mDragTarget :: Maybe DragTarget
- startMouseRatio :: Number
- startSidebarRatio :: Number
- startPreviewRatio :: Number
- sidebarRatio :: Number
- previewRatio :: Number

### Toggle visibility

Since all other components of EditorPage are in Splitview as children, toggling their visibility is thus also a responsibility of Splitview. As the middle main [editor](Editor.md) view is always visible, there does not exist the ability to close this component. The other two views can be closed by the respective button on their resizer to close and reopen themselves. The right view additionally has a close button on its top right to close itself. However, each view can only have one component active at a time. The content of the right view can be changed depending on different button presses. The left view, however, can have "overlapping" components, which can be closed with a top-right close button. The order from bottom to top is the following: TOC, CommentOverview, and then Comment. The components are made invisible, but they are not deleted, so as not to delete their state and reload the information again if we open them up again.

#### Toggle visibility uses the following Actions:

- ToggleComment
- ToggleCommentOverview Boolean Int Int
- ToggleSidebar
- TogglePreview

#### Toggle visibility uses the following state labels:

- sidebarRatio :: Number
- previewRatio :: Number
- lastExpandedSidebarRatio :: Number
- lastExpandedPreviewRatio :: Number
- sidebarShown :: Boolean
- tocShown :: Boolean
- commentOverviewShown :: Boolean
- commentShown :: Boolean
- previewShown :: Boolean
