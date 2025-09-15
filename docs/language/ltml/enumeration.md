# Enumerations

Enumerations are composed of a list of [textual](./text.md) items, which are
written as a consecutive sequence of [text children](./text.md#child-nodes),
each headed by a keyword that is specific to the enumeration type.


## Enumeration text

Enumeration text permits [styling](./text.md#styling),
[footnote references](./text.md#footnote-references), and (nested)
enumerations.


## Enumeration identifiers

Enumeration items get assigned a sequential enumeration
[identifier](general/identifier.md#output-identifiers) (e.g., a number).
Optionally---depending on the enumeration type---this identifier is
automatically printed next to the respective item in the output
(c.f. LaTeX' `enumerate` and `itemize` environments).

The enumeration identifiers can always be produced by
[referencing](./text.md#references) the respective items.


## Nesting

Enumerations may indirectly contain sub-enumerations:
the text within an enumeration may contain an enumeration.


## Example

```
Some sentence with
  # one enumeration item,
  #{itm:} a labeled item ({:itm}),
  # another item,
    which spans
    multiple lines,
  # an item
      # with
      # sub-items
    and more text,
  #
    and an item starting in a new line.
Another sentence
  # has a single item.
```

Assuming an enumeration type with keyword `#` and numbered (`N.`) output, we
get:

```
Some sentence with
  1. one enumeration item,
  2. a labeled item (2),
  3. another item, which spans multiple lines,
  4. an item
       1. with
       2. sub-items
     and more text,
  5. and an item starting in a new line.
Another sentence
  1. has a single item.
```
