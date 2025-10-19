# FPO.Components.Editor

There are two types of editor. The editor, to which the [splitview](51-Splitview.md) can access with id 0, is the main editor in which the user can edit and work inside of it. The other editor, which the parent can access with id 1, is used to compare older versions of the document with the main editor. It is, however, set to read-only and its content cannot be changed. Since this component has a lot of features and is quite long, many data types and functions are relocated into FPO.Component.Editor.Types.

## Main editor

At the beginning, since no section has been selected, no content is loaded and the editor is set to read-only. When the TOC component selects an entry, it is then redirected to this editor and disables the read-only state. Then it requests the data from the backend with the help of ContentDto. This data contains the content, comment anchors, which are markers for the position of comments, and HTML, which is then sent to the [preview](55-Preview.md) component. The main editor has a lot of features, which are further explained later on.

## Important Interactions

### Toolbar

TODO: Write the toolbar section.

### Request

This component directly loads the content for the selected entry directly from the backend and does not communicate with its parent regarding this issue. The data is a wrapper defined in ContentDto, which contains the content, comments, and rendered preview in the form of HTML. The content is stored in its state and the HTML string is sent to the preview component. The comment process is detailed at a different point. To post data, the editor wraps only the content and comments into one data type, since HTML is only created in the backend.

#### Request uses the following Actions:

- ChangeToSection TOCEntry (Maybe Int) (Maybe String)
- ContinueChangeToSection (Array FirstComment)
- Upload TOCEntry ContentWrapper Boolean

#### Request uses the following state labels:

- mContent :: Maybe Content
- html :: String
- markers :: Array AnnotatedMarker

## Comment

The comment aspect is quite complex, as it has multiple features related to this. It thus has its own state called CommentState, which stores all relevant information in it. The editor loads the information about the comments in two different locations. It gets the positions of its anchors in this component and the extra information from the [comment](53-Comment.md) component. Therefore, when TOC requests an entry change, the corresponding Action is divided into two parts. The first part is to get data from the backend and send an information request to the [comment](53-Comment.md) component. After receiving it, it then processes it in the second part.

### Comment position representation

There exist two arrays that hold different representations of the positions of the comments. The **live markers** are the temporary representation, which the editor actually uses to work with. The positions of the comments are constantly updated by mainly two interactions:

- **Anchor:** For each comment, there exists a start and end anchor, which move according to the content changes made by the user inside the editor. Each comment is then given a custom CSS class to highlight this range in the editor.  
- **Handler:** At most, only one comment can be selected at a time. When selected, each end now has a handle, which the user can use to change its range through mouse drag and drop.

The other data type is called **AnnotatedMarker**. It is an intermediate representation of the live markers. It converts the live markers to JSON or the other way around.

### Modifying the range of a comment

As previously mentioned, we have two ways of changing the positions of the ends of a comment. The anchor updates with every text change, but the corresponding CSS class does not. With every Anchor change notification, we replace the old marker with the updated one. The other method is more complicated and uses a lot of Actions and state labels.

#### Handles

Handles are a CSS class that the user can click on and drag. These are, however, only for visualization and can be turned on and off by the two functions **showHandlesFor** and **hideHandlesFrom**. Only selected comments can have those handles. The interactions of actually moving the handles are realized by the three listeners, which react to the events **mousedown**, **mousemove**, and **mouseup**. We use four Actions for the interaction:

- **TryStartDrag:** First check if the clicked position is near the selected comment's start or end handle. If this is true, then put the corresponding **DragHandle**, **mPrevHandler** for auto-save, and **mHandleBorder** for range detection in the commentState and go into Action StartDrag.  
- **StartDrag:** Only able to drag comments if we are the main editor, which can be checked if there is **no compareToElement**. We add the **fpo-dragging** CSS class to prevent the cursor from highlighting the selected text. Then refresh the handles before going to the next Action.  
- **DragMove:** We use an FFI to convert the mouse position to coordinates in the editor. While dragging, it prevents the new position from being beyond the other handles. We store **dragRowAS** and **dragColAS**, with AS for auto-save, to check if the comment range has changed since the last save.  
- **EndDrag:** First, set the **DragState** to Nothing to stop the dragging. Remove the **fpo-dragging** CSS class to allow selecting text highlighting again and remove the selection from the user. Then prepare for potential auto-save.

It has additional **ShowHandles** and **HideHandles** Actions, which are mainly used for the corresponding **showHandlesFor** and **hideHandlesFrom** functions. They are primarily used for easy access by other Actions in this component.

#### Handles use the following Actions:

- TryStartDrag Number Number -- clientX, clientY
- StartDrag DragHandle LiveMarker Number Number -- mouse down: which, lm, clientX, clientY
- DragMove Number Number -- mouse move: clientX, clientY
- EndDrag -- mouse up

#### Handles use the following commentState labels:

- dragState :: Maybe {'{'} which :: DragHandle, lm :: LiveMarker {'}'}
- startHandleMarkerId :: Maybe Int
- endHandleMarkerId :: Maybe Int
- mPrevHandler :: Maybe Types.Position
- dragRowAS :: Int
- dragColAS :: Int
- mHandleBorder :: Maybe HandleBorder

### Annotated Marker sequence

After loading the comment from ContentWrapper in the GET request, it is then converted to an AnnotatedMarker and stored in markers. Since we need more information from the comments, we request the first message of the comment from the [comment](53-Comment.md) container at the end of the first Action. In the continuation Action, we filter the acquired data to have only non-resolved comments. Then, with the help of the **addAnchor** function, the annotated marker is converted to live markers, added to the editor, and stored in the state.

### Annotation

On the gutter on the left side of the editor, it shows the comment annotation. It shows where the comments start in the editor. When hovering over the icon, it also shows the name of the author who wrote the first message in the comment conversation. If there are multiple comments starting on the same line, all different users are listed in the annotation. Same users on the line are compressed in the format: user+int. For the latter feature, we use two hashmaps.

- **markerAnnoHS:** Takes the line of the editor as its key and returns as value another hashmap. The returned hashmap represents the occurrence of the user.  
- **oldMarkerAnnoPos:** Used to find out on which line the comment lies. It uses the ID of the marker as the key, and the returned row number can then be used in markerAnnoHS.

### Creating Comment

The user first selects a text and then clicks on the Comment button. This triggers the **Comment** Action. There it gets the data **user** to use its name later. It converts the selected text into a range, from which the start and end positions are extracted. From this data, it creates a new **AnnotatedMarker**. With the help of **addAnchor**, it converts it into a **liveMarker**. This marker is then stored as the current selected **liveMarker** and as a temporary liveMarker **tmpLiveMarker**. Temporary because we do not want to send it to the backend until the first message is sent in its conversation. When a **tmpLiveMarker** already existed prior to clicking the Comment button, it just overwrites **tmpLiveMarker** with the new one. After saving the new marker in the state, it then sends a notification **AddComment** towards the [comment](53-Comment.md) component. It then shows the handles of the **tmpLiveMarker**, as it is selected, and clears the marked selection. When the first message has been sent, this editor receives a query **UpdateComment**.

#### Creating Comment uses the following Actions/Output/Query:

- Comment  
- AddComment  
- UpdateComment

#### Creating Comment uses the following state labels:

- markerAnnoHS
- oldMarkerAnnoPos
- tmpLiveMarker
- selectedLiveMarker
- markers
- liveMarkers

## Saving

There are two ways to save in the editor: **manual saving** and **auto-saving**. Both of them have the same sequence of sending the **content** and the **positions of the comments** in this section to the backend. The only difference is that manual saving is done with the save button and other interactions, while auto-save is done with a timer. It also sends an **autoSave boolean** value to the backend, which is used for compare editor. It can only be sent if the mutable value reference **mDirtyRef** is set to true. This action occurs in the added listener with the function **addChangeListenerWithRef**, which sets multiple flags to true and starts a timer for auto-save. Since saving uses a lot of state labels, it has a separate type called **SaveState**.

### Saving

The user is able to save the changes with the help of the save button. It only works if the mutable value reference **mDirtyRef** is set to true beforehand. There we first go into the **Save** Action, where it first checks if it is the **main editor** or not. Only the main editor has the ability to save the changes. It then checks the dirty flag. Only if the previous checks have turned to true does it start to collect all the necessary data before uploading it. It first extracts the text from the editor and updates the **content** value, stored in **SaveState**. Afterwards, it then updates the positions of the **array of AnnotatedMarker called markers** from its corresponding **array of LiveMarker called liveMarkers**, with each **LiveMarker** having the same ID as its matching **AnnotatedMarker**. With both pieces of data, it then puts them into a wrapper defined in **ContentDto**. It then goes into the next Action **Upload**. (With this separation, we can call **Upload** again in case of a failed upload attempt, without extracting the data from the editor again. This is, however, deprecated.) With the data, it converts them into JSON and sends it to the backend. When an error is sent back, it displays it in a toast.  
TODO: other cases  
At the end, it sends the rendered HTML to the [preview](55-Preview.md) component and updates the section title displayed above the editor, which is requested from the TOC component.

#### Saving uses the following Actions/Output:

- Save
- Upload
- SavedIcon
- RequestFullTitle

#### Saving uses the following state labels:

- markers
- liveMarkers
- mDirtyRef
- showSavedIcon
- mSavedIconF

### Auto Save

With every change in the text, the **dirty flag** is set to true and goes into the **AutoSaveTimer** Action. There, two timers are started with the help of two fibers: a 2-second and a 20-second timer. The shorter timer is reset every time the user makes another change. The longer one is used to save the changes regardless of whether the user has made some changes or not. When either of the timers has run out, it then goes into the Action **AutoSave**. There, it kills both fibers to stop the timers and goes into the **Save** Action, which is already described above.

#### Auto Save uses the following Actions:

- AutoSaveTimer
- AutoSave
- Save

#### Auto Save uses the following state labels:

- mDirtyRef
- mPendingDebounceF
- mPendingMaxWaitF

### The following actions triggers save:

- Save button
- Background timer of 20 seconds or 2 seconds after latest content change
- Change to a diffrent Leaf
- Closing tab
- Send first message in a new comment
- Change border of a comment
- 
TODO: Older versions of section.
