# Legal Text Markup Language (LTML)

LTML is a language designed to write legal texts, specifically German
"Fachprüfungsordnungen", but to be flexible enough to use for any kind of
structured text.

The flexibility is achieved via the
[LTML Schema Definition (LSD) language](lsd.md),
which defines types for LTML nodes and how these may be composed.

LTML documents are written partially via a GUI, and partially via a text
editor.

The syntax (of the text portions) is inspired by
[Markdown](https://commonmark.org), among others.

LTML document containers are to be converted to an output format
(specifically, PDF and HTML).


## Structure

LTML defines [document containers](ltml/document-container.md), which are
made up of [documents](ltml/document.md), but are sometimes themselves
referred to as documents.

See also:

* [Syntax (outdated)](ltml/syntax.ebnf.txt)


## Labels & references

* Nodes can generally be [labeled](ltml/general/label.md), and
  [referenced](ltml/text.md#references) in text using those labels.


## Comments

LTML documents permit C-style line comments; that is, two forward slashes
(`//`) generally cause text until the end of line to be ignored.
This does not work within language constructs enclosed in curly brackets.
