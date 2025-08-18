# Documents

An LTML document is composed of:

* a [header](#header): some metadata
* a [body](#body): a tree of *nodes*
* a [set of labeled footnotes](#footnotes)


## Header

The header may contain the following nodes.

* Title


## Body

The body is composed of:

* an intro: a sequence of fixed-type [simple sections](./simple-section.md)
* a main part: a [section body](./section.md#body)
    * One may thus see a document as a fancy section.
* an outro: a sequence of fixed-type [simple sections](./simple-section.md)


## Footnotes

* In the AST, [footnotes](./footnote.md) are part of the document in the form
  of a map from [labels](./general/label.md) to footnotes.
* In the input, these occur within leaf [sections](./section.md) or leaf
  [simple sections](./simple-section.md) that are part of the document's body.
* In the output, footnotes are placed [appropriately](./footnote.md).
