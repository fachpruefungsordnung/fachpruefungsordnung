# Documents

An LTML document is composed of a [header](#header) and a [body](#body),
where the former contains some metadata and the latter is a tree of *nodes*.
The permitted header nodes, body nodes, and their composability are determined
by an [LTML Schema Definition](../lsd.md).
Body nodes can be [labeled](./general/label.md).

Generally, only parts of an LTML document are represented as human-readable
text; the nodes close to the root are generally only editable via a GUI.  This
applies to document nodes in particular.

The syntax is inspired by [Markdown](https://commonmark.org), among others.


## Header

The header may contain the following nodes.

* Title
* Output header (displayed on the top of each page)
* Output footer (displayed on the bottom of each page)


## Body

The body is composed of [sections](section.md).
