# Sections

Sections have an [unstyled-textual heading](#heading-text) and a [body](#body).

The heading and body are separated by at least one linebreak.

Sections are identified by [keywords](general/identifier.md#input-identifiers),
which precede the heading.

Compare Markdown's different headings (`#`, `##`),
MediaWiki's sections (`==`, `===`, ...), and
LaTeX' sections (`\section`, `\subsection`, ...).


## Heading text

Heading [text](./text.md) permits neither [styling](./text.md#styling) nor
[enumerations](./enumeration.md), but
[footnote references](./text.md#footnote-references).

Further, heading text is [headed](./text.md#keyword-headed-text) by the
(possibly [labeled](./general/label.md)) section keyword
(i.e., one level of indentation is implicitly added---noticeable with heading
text that spans multiple lines).


## Body

The body is a fixed-type sequence of any of:

* lower-level sections
* [paragraphs](./paragraph.md)
* [simple blocks](./simple-block.md)

These body elements may be separated by any number of empty lines from each
other.

Additionally, if the section is a leaf section (that is, does not contain
sub-sections, but only either paragraphs or simple blocks), it may contain
[footnotes](./footnote.md) in place of any paragraph or simple block.
These footnotes do not become part of the section, but rather the encompassing
[document](./document.md).


## Example

```
={main:} Main

§{section_a:} Some section

This paragraph is in {:section_a} in super-section {:main}.

This is another paragraph in {:section_a}.
Paragraphs don't have keywords and are just separated by empty lines.

§{section_b:} Another section, with a title
  spanning
  several lines
  and a footnote{^:fn}

This paragraph is in {:section_b} in super-section {:main}.

^{fn:} The heading's footnote.
```

This assumes two section types, one with keyword `=` and another with keyword
`§`, where the former may contain the latter, and the latter may contain
paragraphs.

See also the examples for [paragraphs](./paragraph.md) and general
[text](./text.md).
