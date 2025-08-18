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

Paragraph text permits all of [styling](./text.md#styling),
[footnote references](./text.md#footnote-references), and
[enumerations](./enumeration.md).

Additionally, paragraph text extends text with [sentences](#sentences).


## Sentences

Text within a paragraph is split into sentences,
Sentences are generally terminated by a single period (`.`), exclamation mark
(`!`), or question mark (`?`), each.
Additionally, they are terminated by enumeration items (with the next sentence
starting after the sequence of enumeration items).

Sentence termination can be undone by inserting the continuation token `{>}`
where otherwise a new sentence might start.
This is meant for continuing a sentence after an enumeration, where it is to
be inserted at the start of the first line after the enumeration.
Otherwise, one may also simply escape the line terminator,
s.t., e.g., `X\. Y.` is equivalent to `X. {>} Y.`).

Sentences are not full nodes; in particular, [styling](./text.md#styling) may
overlap with sentences.

However, sentences may be [labeled](./general/label.md) at their beginning
(and [referenced](./text.md#references)).
[Paragraph labeling](#labeling) takes precedence over sentence labeling; to
label a sentence at the start of a paragraph (and best always), write the
labeling on the same line as the start of the sentence.


### Enumerations

There should be at most one [enumeration](./enumeration.md) per sentence;
otherwise, it is generally impossible to properly reference an enumeration
item, for the enumeration item output [identifiers](./general/identifier.md)
would generally be reused within the sentence.
This is not enforced; i.e., it is the responsibility of the user.

Note that adding multiple enumerations to a sentence requires either using
continuation tokens (see above) or using enumerations of different types in
direct succession.


## Example

```
{paragraph_a:}
This paragraph uses all text features in a minimalistic way.
Some of this text is <*bold>, </cursive>, <_underlined>.
This is an enumeration:
  # enumeration item 1
  # enumeration item 2
    # nested enumeration item
Next, we use a footnote{^:fn}.
{sentence_a:} This sentence has a label.
This is a reference to the preceding sentence: {:sentence_a}.
This sentence spans
multiple
lines.  {sentence_b:} This sentence starts in-line.
```

This assumes an enumeration keyword `#`, and an elsewhere-defined footnote
with label `fn`.

See also the examples for general [text](./text.md).
