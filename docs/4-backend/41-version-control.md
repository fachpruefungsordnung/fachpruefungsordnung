# Version Control & Document Management

The Backend provides an API to create, edit, manage and version documents.
The documents can be stored in any database, the current implmenetation uses PostreSQL.

## Document Management

A FPO instance can manage multiple documents. Each document is owned by a *group*.
Documents are visible and editable by each *member* of the group.
External reviewers and editors can be manually added to a document.

Admins and Superadmins can manage their groups and, consequently, the projects within those groups. Core functionalities include:

* Creating new projects
* Archiving projects
* Deleting projects
* Assigning document permissions to users

Regular users of the application receive an overview of the projects they are assigned to upon login and can access them based on their permissions.

- Documents are assigned to a single group
- All group members always have full access to all documents within the group
- Individual external users can be added to collaborate on specific documents of the group
  - The following permission levels exist: `Read`, `Review`, `Edit`

|                      | Read | Review | Edit |
|----------------------|:----:|:------:|:----:|
| Read                 |✅    |✅     |✅    |
| Comment              |❌    |✅     |✅    |
| Write                |❌    |❌     |✅    |
| Rearrange Paragraphs |❌    |❌     |✅    |

Commenting also includes suggesting changes.

For more detailed information, see [user management](user-management).

## Documents

A documents consists of a *tree structure* and *text elements*.
The tree structure is closely resembled by the Table of Contents.
The structure of a document and the contents of the text elements are strictly separated.
Text elements are referenced in leaf nodes of the structure tree only.
The leaf nodes only contain a reference to a text element, but do not contain any actual text.

## Version Control

Consequently, the overall structure of the document, the structure tree, has a
revision history, but all text elements also have their own revision histories.
Thus, there are two kinds of revision: *tree revisions* and *text revisions*.
The API also exposes a third kind of revision, a *document revision*.
Document revision however do not exist as such, each document revision is either a
tree or a text revision.
Document revisions are used to obtain all revisions made to a document in chronological order.
For more information, see [GET /docs/\{documentID\}/history](https://fpo.bahn.sh/swagger/#/default/get_docs__documentID__history)

### Noteworthy Implications

This version control model has some implications worth noting:

A revision to the structure tree does not contain any information on specific text
element revisions. It is however possible to obtain the full document including text
element revision at the state of a specific revision, either tree or text
(see [GET /docs/rev/\{revision\}](https://fpo.bahn.sh/swagger/#/default/get_docs__documentID__rev__revision_) and all other endpoints at `/docs/rev`).

A text element has to be valid for each tree version it is part of.
Thus, a text elements *kind* and *type* (for more information, see language) are immutable.
To guarantee this immutability, the type and kind of a text element are not part of the text revisions but the text element itself.

A revision is always restricted to either a documents structure tree or a specific text element.
Thus, it is not possible to edit multiple text elements in one revision.
It is also not possible to edit a text element and the structure tree in one revision.
This forces the user to make more atomic revisions, but it might also add potentially unnecessary noise in some cases.

### Reasons for the Choosen Model

This model was chosen for the following reasons:

Due to the restrictiveness and atomicy of revisions, it is easilly possible to create revisions automatically.
To prevent unnecessary pollution of the revision history, subsequent revisions by the same author to the same element are squashed within a set time frame.

Forcing the user to restrict changes to one text element while automatically creating new revisions frequently, thus keeping revisions as atomic as possible, lowers the possibility of a conflict.

Adding real-time collaborative features for text editing later does not require any additional separations of text buffers, since it is already present in the underlying model.
