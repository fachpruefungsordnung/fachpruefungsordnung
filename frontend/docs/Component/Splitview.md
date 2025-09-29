# FPO.Components.Splitview

Splitview is the main component, that is used in EditorPage. It is the parent component and have multiple child components, split in 3 sperated views. Depending on the task, the child could be displayed either on the left, middle or the right side of the splitted view. The splitview component is thus the coordinator for all the children. It either redirects data from one component to another (e.g. selecting an entry in TOC should notify the [editor](Editor.md) component via sending the corresponding ID.), only getting data from its children (e.g. closing request from Comment component) or only sending data to child. (e.g. sending newly received tree from backend to TOC.) The middle view only has the main [editor](Editor.md) and is always visible. The other views can each only harbor one child component at the time and can also be closed by the user.

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
  - 
## Important Interactions

### Request

Splitview is the component responsible on handling the requests regarding the document tree from and to backend. In GET request the tree is then stored in the state as tocEntries and then sent to TOC. The POST is only called by TOC to update the new structure of the tree to the backend.

#### Request uses the following Actions:

- GET
- POST

#### Request uses the following state labels

- tocEntries
- versionMapping

### Resize

The splitview gives the user the ability to resize the size of the 3 splitted views. It uses a DOM-Element and refer here as resizer. The resizer visualize the border of the middle view with either one of the other view. With the mouse events, the user can drag and drop the resizer to resize the sizes, which is calculated in percentage. While dragging, the views cannot be smaller than a set size, which is 5%. While the middle and right views max size is dependable on the min sizes of the other views, the left view max size is capped at 20%.

#### Resize uses the following Actions:

- StartResize DragTarget MouseEvent
- StopResize MouseEvent
- HandleMouseMove MouseEvent
- 
#### Resize uses the following state labels

- mDragTarget :: Maybe DragTarget
- startMouseRatio :: Number
- startSidebarRatio :: Number
- startPreviewRatio :: Number
- sidebarRatio :: Number
- previewRatio :: Number

### Toggle visibility

Since all other components of EditorPage is in splitview as children, toggling its visibility is thus also a responsibility for spilitview. As the middle main [editor](Editor.md) view is always visible. There does not exists the ability to make this component close. The other two views can be closed by the respective button on their resizer to close and reopen itself. The right view has additionally on its top right a close button to close itself. However each view can only have one component at the time active. The content of right view can be changed, depending on different button presses. The left view however can "overlapping" components, which can be closed with a top right close button. The order from bottom to the top is the following: TOC, CommentOverview and then Comment. The components are made invisible, but they are not deleted as to not delete its state and loading the information again, if we open them up again. 

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