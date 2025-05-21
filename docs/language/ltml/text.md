# Text

Some nodes contain *text*, which may be [styled](#styling), and contain
[references](#references) and [child nodes](#child-nodes).

Text may be spread over several lines; linebreaks are translated to single
ASCII space characters.
Empty lines are disallowed.

If headed by a node [keyword](general/identifier.md#input-identifier),
text implicitly adds one level of [indentation](general/indentation.md).
The first line may generally be written right after the keyword, without extra
indentation.
[Sentence](./block.md#Sentences) terminators do not count as keywords in this
context.


## Styling

* Nodes may permit styled text.
* Specifically, text may be `<*bold*>`, `</in italics/>`, and/or
  `<_undelined_>`.
* Different style tags may be nested (e.g., `<*bold </and italics/>*>`), but
  not the same (e.g., `<*bold <*again*>*>` is illegal).


## References

[Labeled](general/label.md) nodes may be referenced within text, using the
respective labels.
In the output, the respective output [identifier](general/identifier.md) is
printed.


## Child nodes

Text nodes (e.g., headings, sentences) generally permit in-line children.
That is, at any point in such a text node, certain children nodes may be
inserted.
This requires breaking the line where the children are to be inserted.

Text children must be [indented](general/indentation.md) one level from the
current context (text);
that is, two levels from the textual node's keyword, if any.

There are several kinds of text children:

* [lists](./lists.md)
* footnotes (TODO)
    * C.f. LaTeX' `\footnote` command.
