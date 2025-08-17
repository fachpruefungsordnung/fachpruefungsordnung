# Simple sections

Simple sections are composed of [simple paragraphs](./simple-paragraph.md).

Unlike regular [sections](./section.md), they do not have a heading or
identifier.

The simple section's children are separated by any number of linebreaks.

Simple sections are identified by
[keywords](general/identifier.md#input-identifiers), which occur on their own
line, preceding the simple section's children.


## Example

```
[intro]

This is a simple paragraph.
```

This assumes a simple section type with keyword `[intro]`.
