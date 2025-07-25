# Text

Some nodes contain *text*, which may generally be [styled](#styling), and
contain [references](#references), [enumerations](./enumeration.md), and
[footnotes](./footnote.md).


## Text kinds

There are several text kinds (e.g., paragraph text), which are determined by
where text occurs, but do not depend on node types.

The text kind determines whether styling and enumerations within text are
permitted, each, and may further extend text.

References and footnotes are always permitted.


## Line breaks & Whitespace

Text may be spread over several lines---maintaining indentation if any.
Such input lines form a single logical line, joined by whitespace.

Any whitespace---either from a single linebreak as described above, or a
non-empty sequence of ASCII spaces---is treated the same: as a word separator,
which is generally rendered as a single space character.
In particular, it is impossible to encode a line break; lines are
automatically broken in the output whenever an output line is full.

Empty lines are disallowed within text.
Note, however, that empty lines may be used to split up
[paragraphs](./paragraph.md), which are otherwise basically just text.


## Keyword-headed text

In some contexts (e.g., [enumerations](./enumeration.md)), text is headed by a
[keyword](general/identifier.md#input-identifiers).

In this case, one level of [indentation](general/indentation.md) is implicitly
added.

The first line may generally be written right after the keyword (in the same
line, separated by at least one ASCII space); otherwise, it starts on the
subsequent line, indented.

See [enumerations](./enumeration.md) for example input.


## Styling

* Some (but not all) node kinds permit styled text.
* Specifically, text may be `<*bold>`, `</in italics>`, or `<_underlined>`.
* Different style tags may be nested (e.g., `<*bold </and italics/>*>`).
    * However, a style tag may not be (transitively) nested within itself
      (e.g., `<*bold <*again*>*>` is illegal).
        * Note: This restriction is not yet implemented and might be removed
          later.
* ```
  <*Styled text can
  span multiple lines,
    ^ contain footnotes,
    # contain enumeration items,
  and continue afterwards---at the same indentation level.>
  ```


## References

[Labeled](general/label.md) nodes may be referenced within text, as
`{:LABEL}`, where `LABEL` is the respective label.
In the output, the respective output [identifier](general/identifier.md) is
substituted.


### Example

```
{p:}
This is a labeled paragraph.

This is a reference to paragraph {:p}.
```

Output:

```
This is a labeled paragraph.

This is a reference to paragraph 1.
```


## Child nodes

Text nodes (e.g., headings, sentences) generally permit in-line children.
That is, at any point in such a text node, certain child nodes may be inserted.
This requires breaking the line where the children are to be inserted.

Text children must be [indented](general/indentation.md) one level from the
current context (text); that is, two levels from the textual node's keyword,
if any.

There are currently two kinds of text children,
[enumeration](./enumeration.md) items and [footnotes](./footnote.md).


### Example

```
Everybody loves these fruits:
  ^ Citation needed.
  # Apples, unless
      # unripe, or
      # not tasty
  # Bananas,
    if yellow
  # Oranges
```

This example is to be read as containing a single footnote and a nested
enumeration.
