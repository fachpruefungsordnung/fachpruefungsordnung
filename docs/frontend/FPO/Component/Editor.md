# FPO.Components.Editor

There are two types editor. The editor, to which the [splitview](Splitview.md) can access with id 0, is the main editor in which the user can edit and work inside of it. The other editor, which the parent can access with id 1, is used to compare older version of the document with the main editor. It is however set to readonly and its content cannot be changed. Since this component has a lot of features and is quite long, a lot of data types and functions are relocated into FPO.Component.Editor.Types.

## Main editor

TODO: Rewrite this.
At the beginning, since no section has been selected, no content is loaded and the editor is set to readonly. When the [TOC](TOC.md) component selected an entry, it is then redirected to this editor and disables the readonly state. Then it requests the data from the backend with the help of [ContentDto](ContentDto.md). This data contains the content,comment anchors, which are markers for the position of comments and html, which is then sent to [preview](Preview.md) component. The main editor has a lot features, which are further explained later on.

## Important Interactions

### Request

This component directly loads the content for the selected entry directly from backend and does not communicate with its parent regarding this issue. The data is a wrapper defined in [ContentDto](ContentDto.md) which contains the content, comments and rendered preview in form of html. The content is stored in its state and the html string is sent to the preview component. The comments process are detailed on different point. To post data, the editor wraps only the content and comments into one data type, since html is only created in the backend. 

#### Request uses the following Actions:

- ChangeToSection TOCEntry (Maybe Int) (Maybe String)
- ContinueChangeToSection (Array FirstComment)
- Upload TOCEntry ContentWrapper Boolean

#### Request uses the following state labels:

- mContent :: Maybe Content
- html :: String
- markers :: Array AnnotatedMarker

## Comment

The comment aspect is quite complex, as it has multiple features related to this. It thus has its own state called CommentState, which stores all relevant information in it. The ediotr loads the information about the comments in two different locations. It gets the positions of its anchors in this component and the extra information from [comment](Comment.md) component. Therefore, when [TOC](TOC.md) request an entry change, the corresponding Action is devided into two parts. The first part is to get data from backend and send an information request to [comment](Comment.md) component. After receiving it, it then process it in the second part.

### Comment position representation

There exists two arrays which holds different representations of the positions of the comments. The **live markers** are the temporary representation, which the editor actually use to work with. The positions of the comment are constantly updated by mainly 2 interactions:

- Anchor: For each comments, there exists a start and end anchor, which moves according to the content changes, made by the user inside the editor. Each comment is then added a custom CSS class to highlight this range in editor. 
- Handler: At max only one comment can be selected at the time. When selecting, each ends now has a handle, which the user can use to change its range through mouse drag and drop.

The other data type is called **AnnotatedMarker**. It is an intermediate representation of the live markers. It converts the live markers to JSON or the other way around.

### Modifying the range of comment

As previously mentioned, we have two ways of changing the positions of the ends of a comment. The anchor updates with every text change, but the corresponding CSS class not. With every Anchor change notification, we replace the old marker with the updated one. The other method is more complicated and uses a lot of Actions and state labels. 

#### Handles

Handles are a CSS class, which the user can click on and drag. These are however only for visualisation and can be turned on and off by the the two functions showHandlesFor and hideHandlesFrom. Only selected comments can have those handles. The interations of actually moving the handles is realized by the three listeners, which react on the events **mousedown**, **mousemove** and **mouseup**. We use 4 Actions for the interaction:

- TryStartDrag: First check, if the clicked position is near the selected comment's start or end handle. If this is true, then put the corresponding **DragHandle**, **mPrevHandler** for auto save and **mHandleBorder** for range detection in the commentState and go into Action StartDrag
- StartDrag: Only able to drag comments, if we are the main editor, which can be checked, if there is **no compareToElement**. We add **fpo-dragging** CSS class, to prevent the cursor from highlighting the selected text. Then refresh the handles before going to the next Action.
- DragMove: We use a FFI to convert mouse postion to coordinates in the editor. While dragging, it prevents the new position to be beyond the other handles. We store **dragRowAS** and **dragColAS**, with AS for auto save, to check, if the comments range has been changed since last save.
- EndDrag: First, set the **DragState** to Nothing to stopt the dragging. Remove **fpo-dragging** CSS class to be able to select highlighting text and remove the selection from the user. Then prepare for potential auto save.

It has an additional **ShowHandles** and **HideHandles** Actions, which mainly used for the corresponding **showHandlesFor** and **hideHandlesFrom** function. It is primary used for easy access by other Actions in this component.

#### Handles uses the following Actions:

- TryStartDrag Number Number -- clientX, clientY
- StartDrag DragHandle LiveMarker Number Number -- mouse down: which, lm, clientX, clientY
- DragMove Number Number -- mouse move: clientX, clientY
- EndDrag -- mouse up

#### Handles uses the following commentState labels:

- dragState :: Maybe { which :: DragHandle, lm :: LiveMarker }
- startHandleMarkerId :: Maybe Int
- endHandleMarkerId :: Maybe Int
- mPrevHandler :: Maybe Types.Position
- dragRowAS :: Int
- dragColAS :: Int
- mHandleBorder :: Maybe HandleBorder

### Annotated Marker sequence

After loading the comment from ContentWrapper in the GET request, it then converts to AnnotatedMarker and stored in markers. Since we need more information from the comments, we request the first message of the comment from [comment](Comment.md) comtainer at the end of the first Action. In the continuation Action, we filter with the aquired data for only having non resolved comments. Then with the help of the addAnchor function, the annoted marker is converted to live markers and added to the editor and stored in state. 

### Annotation

On the gutter on the left side of the editor, it shows the comment annotation. It shows, where the comments start in the editor. When hovering over the icon, it also shows the name of the author, who wrote the first message in the comment conversation. If there are multiple comments starting in the same line, all different users are listed in the annotation. Same users on the line are compressed in such format: user+int. For the latter feature we use two hashmaps.

- **markerAnnoHS**: It takes the line of the editor as its key and returns as value another hashmap. The returned hashmap represents the occurence of the user.
- **oldMarkerAnnoPos**: To find out, on which line the comment lies. It uses the id of the marker as the key and the returned row number can then be used in markerAnnoHS

### Creating Comment

The user first selects a text and then click on the Comment button. This puts it into the **Comment** Action. There it gets the data **user** to use its name for later. It converts the selected text into range, from which the start and end postions are extracted. From this data, it creates a new **AnnotatedMarker**. With the help of the **addAnchor** it converts it into a **liveMarker**. This marker is then stored as current selected **liveMarker** and as a temporary liveMarker **tmpLiveMarker**. Temporary as we do not want to send it to the backend, unitl the first message is sent in its conversation. When there existed a **tmpLiveMarker** prior to click the Comment button, it just overwrites **tmpLiveMarker** with the new one. After saving the new marker in state it then sends a notification **AddComment** towards the [comment](Comment.md) component. It then shows the handles of the **tmpLiveMarker**, as it is selected and clear the marked selection. When the first message was sent, this editor gets as a query **UpdateComment**. 

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

There are two ways to save in the editor. **Manual saving** and **auto saving**. Both of them have the same sequence of sending the **content** and the **position of the comments** in this section to the backend. The only difference is that manuell is done with the save button and other interactions while auto save is done with a timer. It also sends an **autoSave boolen** value to the backend. It can only be send, if the mutable value reference **mDirtyRed** is set to true. This action occurs in the added listener with the function **addChangeListenerWithRef**, which set multiple flags to true and starts a timer for auto save. Since saving uses a lot of state labels, it has a seperate type called **SaveState**.

### Saving

The user is able to save the changes with the help of the save button. It only works if the mutable value reference mDirtyRed is set to true prior. There we go first in the **Save** action, where it first checks, if it is the **main editor** or not. Only the main editor has the abilty to save the changes. It then checks the dirty flag. Only if the previous checks have turned to true it then starts to collect all the necessary data before uploading it. It first extract the text from the editor and update the **content** value, stored in **SaveState**. Afterwards it then updates the postions of the **array of AnnotatedMarker called markers** from its corresponding **array of LiveMarker called liveMarkers** with each **LiveMarker** having the same ID as its matching **AnnotatedMarker**. With both of the data, it then puts them into a wrapper defined in **[ContentDto](Dto/ContentDto.md)**. It then goes into the next Action **Upload**. (With the seperation we can call **Upload** again, in case of a failed upload attempt, wihtout extracting the data from editor again. This is however decprecated.) With the data it converts them into a JSON and send it to the backend. When an error is sent back, it displays it in a toast. 
TODO: other cases
At the end it sends the rendered html to [preview](Preview.md) component and update the section title, displayed above the editor, which is requested from the [TOC](TOC.md) component.

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

With every change in the text the **dirty flag** is set true and goes into the **AutorSaveTimer** Action. There two timers are started with the help of two fibers. A 2 and 20 seconds timer. The shorter timer is reset every time, when the user made another change. The longer one is used to save the changes regardless whether the user made some changes or not. When either of the timer had run out, it goes then into the Action **AutoSave**. There it kills all both fibers to stop the timers and goes into the **Save** Action, which is already described as above.

#### Auto Save uses the following Actions:

- AutoSaveTimer
- AutoSave
- Save

#### Auto Save uses the following state labels:

- mDirtyRef
- mPendingDebounceF
- mPendingMaxWaitF

TODO: Add save interactions for example closing tab

TODO: older Versions of section/ 

