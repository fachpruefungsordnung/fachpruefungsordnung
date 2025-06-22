/**
 * How to create Regex rules for custom Ace mode:
 * { token: name , regex: rules }
 * 
 * How are the rules created:
 *
 *  The regex is within "/ ... /"
 * 
 * Character Classes & Symbols
 *
 * .        – Matches any single character (except newline)
 *            e.g. /a.b/ matches "acb", "a1b", but not "ab"
 *
 * [...]    – Matches any one character inside the brackets
 *            e.g. /[abc]/ matches "a", "b", or "c"
 *
 * [^...]   – Matches any one character NOT inside the brackets
 *            e.g. /[^0-9]/ matches anything except digits
 *
 * \d       – Matches a digit (0–9), same as [0-9]
 *            e.g. /\d+/ matches "123", "42"
 *
 * \D       – Matches a non-digit, same as [^0-9]
 *            e.g. /\D+/ matches "abc", "?!"
 *
 * \w       – Matches a word character: [a-zA-Z0-9_]
 *            e.g. /\w+/ matches "foo_1"
 *
 * \W       – Matches a non-word character
 *            e.g. /\W+/ matches spaces, symbols, etc.
 *
 * \s       – Matches whitespace (space, tab, newline, etc.)
 *            e.g. /\s+/ matches " ", "\t", "\n"
 *
 * \S       – Matches a non-whitespace character
 *            e.g. /\S+/ matches "word", but not spaces
 *
 *  Anchors (Position Matching)
 *
 * ^        – Matches the beginning of a line
 *            e.g. /^Hello/ matches "Hello" only if it's at the start of the line
 *
 * $        – Matches the end of a line
 *            e.g. /!$/ matches "!" only at the end of a line
 *
 * \b       – Word boundary
 *            e.g. /\bTODO\b/ matches "TODO", but not "TODOS"
 *
 * \B       – Non-word boundary
 *            e.g. /\Boo/ matches "zoo", but not "oo" at the start
 *
 * Quantifiers (Repetition)
 *
 * *        – Matches 0 or more times           */
 //           e.g. /a*/ matches "", "a", "aaa"
 /**
 * +        – Matches 1 or more times
 *            e.g. /a+/ matches "a", "aaa"
 *
 * ?        – Matches 0 or 1 time (optional)
 *            e.g. /a?/ matches "a" or nothing
 *
 * {n}      – Matches exactly n times
 *            e.g. /a{3}/ matches "aaa"
 *
 * {n,}     – Matches n or more times
 *            e.g. /a{2,}/ matches "aa", "aaa", "aaaa"...
 *
 * {n,m}    – Matches between n and m times
 *            e.g. /a{1,3}/ matches "a", "aa", or "aaa"
 *
 * Groups & Alternatives
 *
 * (...)    – Capturing group
 *            e.g. /(ab)+/ matches "ab", "abab", etc.
 *
 * |        – Alternation (OR)
 *            e.g. /yes|no/ matches either "yes" or "no"
 *
 * Escaping Special Characters
 *
 * Some characters (*, ., [, (, \, etc.) have special meaning and need to be escaped:
 *
 * \*       – Matches a literal asterisk "*"
 * \.       – Matches a literal dot "."
 * \\       – Matches a literal backslash "\"
 */

(function() {
  if (!window.ace) return;

  const TextMode = ace.require("ace/mode/text").Mode;
  const TextHighlightRules = ace.require("ace/mode/text_highlight_rules").TextHighlightRules;

  function CustomHighlightRules() {
  this.$rules = {
    start: [
      // --- Headings ---
      { token: "markup.heading.1", regex: /^# .+/ },
      { token: "markup.heading.2", regex: /^## .+/ },

      // --- Style-Tags (LTML-like) ---
      // nested tags are not supported. TODO: Maybe create custom tags for all combinations?
      { token: "markup.bold", regex: /<\*[^>]+?>/ },        // <*bold>
      { token: "markup.italic", regex: /<\/[^/>]+\/>/ },    // </italic/>
      { token: "markup.underline", regex: /<_[^>]+?>/ },    // <_underline>
      // Optional: fallback for any other style tag
      { token: "markup.other", regex: /<[/\*_][^>]+?>/ },

      // --- Comments ---
      { token: "comment", regex: /^\-\-.*$/ },              // -- single-line comment
      { token: "comment", regex: /\/\*/, next: "comment" }, // /* multiline start

      // --- Keywords ---
      { token: "keyword", regex: /\b(TODO|FIXME|NOTE)\b/ }  // TODO, FIXME, NOTE
    ],

    comment: [
      { token: "comment", regex: /\*\//, next: "start" },
      { token: "comment", regex: /.*/ }
    ]
  };
    this.normalizeRules();
  }
  CustomHighlightRules.prototype = Object.create(TextHighlightRules.prototype);
  CustomHighlightRules.prototype.constructor = CustomHighlightRules;

  function CustomMode() {
    this.HighlightRules = CustomHighlightRules;
  }
  CustomMode.prototype = Object.create(TextMode.prototype);
  CustomMode.prototype.constructor = CustomMode;

  ace.define("ace/mode/custom_mode", [], function(require, exports, module) {
    exports.Mode = CustomMode;
  });
})();
