# Labels

Body nodes may generally be labeled, in which case one may
[reference](../text.md#references) them.

Labeling is only possible if the respective node has an
[output identifier](./identifier.md#output-identifiers).


## Label syntax

A label must start with a lower-case ASCII letter, and is otherwise composed
of lower-case ASCII letters, ASCII digits, `_`, and `-`.


## Labeling syntax

A labeled node is written as `KEYWORD{LABEL:} CONTENT`, where

* `KEYWORD` is empty if the respective node does not have a keyword, and
* `CONTENT` is separated from the labeling by any amount of whitespace, which
  generally may contain at most one newline.

Special rules apply for [paragraphs](../paragraph.md#labeling) and
[sentences](../paragraph.md#sentences).
