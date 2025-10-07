# FPO.Components.Comment

The comment module is used to showcase the **conversation of a comment**. This comment is selected in the **[editor](Editor.md)** module and showcased it on the left side in the **[splitview](Splitview.md)**.

### Showing Comment

A comment is a conversation, where users can send more messages to the previous ones. It follows the logic where the most recent message is put at the end of this conversation. There are two types of comments. The **first comment** and the later ones. Whenever the user creates a new comment in **[editor](Editor.md)** module, there does not exist any messages yet. The first message has thus beside the **send button**, to send a messsage, a **delete button**. When clicking the delete button, a prompt appears to ask the user, if he really wishes to delete this comment. After sending the first message, this one is diplayed bigger and in a more brighter and yellowisher tone then the later ones. Then the future messages have instead of the **delete** a **resovle button** to "close" this conversation. After resolving, the first message has now a green background and the user cannot write more messages.

#### Showing Comment uses the following Actions:

- SendComment
- ResolveComment
- DeleteComment
- RequestModal Mode
- CancelModal

### Draft and Sending Comment

When writing a new message for the conversation, its draft is then stored in its state. It only allows to send a message, if its content is not empty. Depending if this draft belongs to a first message or not, it uses a different API. 

#### Draft and Sending Comment uses the following Actions:

- UpdateDraft String
- SendComment