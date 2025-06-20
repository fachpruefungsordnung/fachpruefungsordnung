# Sections

Sections have an [unstyled-textual heading](#heading-text),
and are composed of lower-level sections, [paragraphs](./paragraph.md),
*tables* (TODO) and/or [appendices](./appendix.md) (TODO).

The heading and section children are separated by any number of linebreaks.

Sections are identified by [keywords](general/identifier.md#input-identifiers),
which precede the heading.

Compare Markdown's different headings (`#`, `##`),
MediaWiki's sections (`==`, `===`, ...), and
LaTeX' sections (`\section`, `\subsection`, ...).


## Heading text

Heading [text](./text.md) permits neither [styling](./text.md#styling) nor
[enumerations](./enumeration.md).

Further, heading text is [headed](./text.md#keyword-headed-text) by the
(possibly [labeled](./general/label.md)) section keyword
(i.e., one level of indentation is implicitly added---noticeable with heading
text that spans multiple lines).


## Example

```
={main:} Main

§{sectionA:} Some section

This paragraph is in {:sectionA} in super-section {:main}.

This is another paragraph in {:sectionA}.
Paragraphs don't have keywords and are just separated by empty lines.

§{sectionB:} Another section, with a title
  spanning
  several lines
    ^ Also, a footnote.

This paragraph is in {:sectionB} in super-section {:main}.
```

This assumes two section types, one with keyword `=` and another with keyword
`§`, where the former may contain the latter, and the latter may contain
paragraphs.

See also the examples for [paragraphs](./paragraph.md) and general
[text](./text.md).
