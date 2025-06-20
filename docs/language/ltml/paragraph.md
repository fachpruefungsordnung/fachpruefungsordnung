# Paragraphs

Paragraphs are composed of [text](./text.md).

Unlike most other nodes, paragraphs are not introduced by keywords.  They are
just [text](./text.md), and separated by (one or more) empty lines.

```
This is a simple paragraph.

This is another paragraph.  With another sentence.
And another.
```

In the output, however, paragraphs are not merely seperated by whitespace, but
each each paragraph is provided with its
[output identifier](./general/identifier.md#output-identifiers)
(typically, a number).


## Labeling

Paragraphs are special w.r.t. [labeling](./general/label.md), in that the
labeling is to be written on its own line, directly preceding the paragraph:

```
{example:}
Some paragraph.

Another paragraph referring to paragraph {:example}.
```

See also [sentence](#sentences) labeling.


## Paragraph text

Paragraph text permits both [styling](./text.md#styling) and
[enumerations](./enumeration.md).

Additionally, paragraph text extends text with [sentences](#sentences).


## Sentences

Text within a paragraph is split into sentences,
Sentences are generally terminated by a single period (`.`), each.
Exceptions apply in the context of enumerations (TODO).

Sentences are not full nodes; in particular, [styling](./text.md#styling) may
overlap with sentences.

However, sentences may be [labeled](./general/label.md) at their beginning
(and [referenced](./text.md#references)).
[Paragraph labeling](#labeling) takes precedence over sentence labeling; to
label a sentence at the start of a paragraph (and best always), write the
labeling on the same line as the start of the sentence.

Further, sentences introduce a context w.r.t.
[identifiers](./general/identifier.md);
specifically, [enumeration](./enumeration.md) items are assigned to sentences,
s.t. a sentence contains at most one enumeration, and an enumeration never
spans multiple sentences.
See also the [enumeration example](./enumeration.md#example).


## Example

```
{paragraphA:}
This paragraph uses all text features in a minimalistic way.
Some of this text is <*bold>, </cursive>, <_underlined>.
This is an enumeration:
  # enumeration item 1
  # enumeration item 2
    # nested enumeration item
Next, we use a footnote.
  ^ This is a footnote.
{sentenceA:} This sentence has a label.
This is a reference to the preceding sentence: {:sentenceA}.
This sentence spans
multiple
lines.  {sentenceB:} This sentence starts in-line.
```

This assumes an enumeration keyword `#` and a footnote keyword `^`.

See also the examples for general [text](./text.md).
