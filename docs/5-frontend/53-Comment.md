# FPO.Components.Comment

The comment module is used to showcase the **conversation of a comment**. This comment is selected in the **[editor](52-Editor.md)** module and showcased on the left side in the **[splitview](51-Splitview.md)**.

### Showing Comment

A comment is a conversation where users can send more messages to the previous ones. It follows the logic where the most recent message is put at the end of this conversation. There are two types of comments: the **first comment** and the later ones. Whenever the user creates a new comment in the **[editor](52-Editor.md)** module, there do not exist any messages yet. The first message thus has, beside the **send button** to send a message, a **delete button**. When clicking the delete button, a prompt appears to ask the user if they really wish to delete this comment. After sending the first message, this one is displayed bigger and in a brighter and more yellowish tone than the later ones. Then the future messages have, instead of the **delete**, a **resolve button** to "close" this conversation. After resolving, the first message now has a green background and the user cannot write more messages.

#### Showing Comment uses the following Actions:

- SendComment
- ResolveComment
- DeleteComment
- RequestModal Mode
- CancelModal

#### Showing Comment uses the following state labels:

- docID :: Int
- tocID :: Int
- markerID :: Int
- commentSections :: Array CommentSection
- mCommentSection :: Maybe CommentSection
- newComment :: Boolean
- commentDraft :: String

### Draft and Sending Comment

When writing a new message for the conversation, its draft is then stored in its state. It only allows sending a message if its content is not empty. Depending on whether this draft belongs to a first message or not, it uses a different API.

#### Draft and Sending Comment uses the following Actions:

- UpdateDraft String
- SendComment

#### Draft and Sending Comment uses the following state labels:

- commentDraft :: String
