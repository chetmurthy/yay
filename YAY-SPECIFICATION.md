# YAY Specification

This specification is for version 0.1.

Parts of this specification have been copied directly from
[the JSON specification](https://tools.ietf.org/html/rfc7159).

Other parts have been copied directly from
[the YAML specification](https://yaml.org/spec/1.2/spec.html).

# Goals of YAY

YAY starts off with the same goals as YAML:

1. YAML is easily readable by humans.
2. YAML data is portable between programming languages.
3. YAML matches the native data structures of agile languages.
4. YAML has a consistent model to support generic tools.
5. YAML supports one-pass processing.
6. YAML is expressive and extensible.
7. YAML is easy to implement and use.

But adds:

8. YAY is specified in terms of familiar lex/yacc concepts, and
   straightforwardly implementable using those tools.
   
9. YAY facilitates perfect specification-compliance from all
   implementations.

10. YAY is *merely* a nicer syntax for JSON.

    In all sense that matter, YAY users can assume that each YAY
    file is equivalent to a JSON file, and YAY implementations can
    produce that JSON file on-demand.  In addition, every JSON file is
    already a YAY file.

# Terminology

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT",
"SHOULD", "SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this
document are to be interpreted as described in
[RFC2119](https://tools.ietf.org/html/rfc4627).

The grammatical rules in this document are to be interpreted as
described in [RFC5234](https://tools.ietf.org/html/rfc5234).

NOTE WELL: I'm aiming for this level of precision, but probably there
are error.

Also, I'm going to use two extensions:

* the character-set-complement operation ("Complement e") below.
  The meaning is this:

  * expression "e" is an expression that specifies a set of characters.
  * the meaning is all characters that are *not* in the
    character-set denoted by "e"

* the character-set subtraction operation ("Subtract (e1, e2)")

  * expressinos "e1", "e2" specify character-sets
  * the meaning is the set of characters specified by "e1", *minus*
    those characters specified by "e2"

In this document, a "text" is a stream of characters to be processed.
A "document" is a stream of characters that express a single YAY
value.  So a YAY text MAY contain one or more YAY documents.  A
YAY text MUST contain at least one YAY document.

# High-Level Description of YAY

For those who are unfamiliar with YAML,
[this](https://yaml.org/spec/1.2/spec.html) is the specification, and
[here](https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started/)
is a YAML tutorial.  There are many such tutorials, and I'm not
recommending this one; just providing an example.

YAY can be described as YAML with the following additions:

* C, C++ comments
* C++ raw-string-literals
* hex, octal number syntax

the following deletions:

* anchors/aliases
* directives (other than an optional first-line version string
* tags
* complex keys, and the "question-colon" ("? ... : ....") syntax for
  expressing them.
* multiline unquoted YAML scalars with whitespace

In addition

* the special characters used to express YAY structure (viz. "[", "]",
  "-", etc) cannot appear in unquoted YAML scalars: when one wants to
  use those, there are always C++ raw-string-literals

* everywhere (outside of quotations) whitespace and newlines are
  insignificant.  At the left margin, spaces are significant and tabs
  are forbidden (b/c easy to make mistakes) but that is all:
  otherwise, whitespace is just insignificant.
  
  This may sound paradoxical, but what matters is *indentation*, not
  whitespace.  Whitespace at the left margin is a form of whitespace
  (and is significant), but elsewhere, whitespace is ignored.

# Syntactic Specification

The syntax of YAY is specified in terms of character set, lexemes, and
a grammar.

## Character Set

The YAY character-set is Unicode.  Implementations MUST accept
UTF-8, and MAY convert other files in other codepages to UTF-8 (rather
than reject).  This specfication does NOT discuss how other codepages
are represented in storage, and it is assumed that those are "transfer
encoding" issue.

## Lexemes

We assume that with each "token" below, comes a "position" (at a
minimum, its offset from the start of the line).  This is trivial to
implement in most lexers, and is sometimes built-in.

The definition of the stream of lexemes produced by the lexer comes in
three phases:

1. define the raw lexemes and tokens.  A token is a lexeme that is
   actually produced by the lexer.  In the following, uppercase
   definitions (e.g. "DECIMAL") correspond to the "tokens"; the other
   definitions are auxiliary.

   We will define three separate groups of lexemes/tokens, which
   naturally leads to an implementation as three entrypoints in lex.

2. define the meaning of a "lesically correct stream".  This can be
   viewed as a bit of state that drives which of the three lexers
   above, are called at any particular moment.

3. define a *transduction* of the stream of tokens from step 2, that
   introduces new synthetic tokens ("INDENT" and "DEDENT").

### White Space

There are three kinds of white space:

```
EOL = "\r" ? "\n"
margin = ' '*
linews = [' ' '\t']
```

and an end-of-input token:

```
EOI = end-of-input or eof
```

### YAY Version Line

YAY texts consist in an optional version-line, followed by a stream
of documents.

If present, the version-line MUST begin at the first character of the
text, and consists in:

```
YAMLVERSION = "YAML-" digit+ "." digit+ eol
```

### Special Characters

```
LBRACKET     = %x5B  ; [ left square bracket
LBRACE    = %x7B  ; { left curly bracket
RBRACKET       = %x5D  ; ] right square bracket
RBRACE      = %x7D  ; } right curly bracket
COLON  = %x3A  ; : colon
COMMA = %x2C  ; , comma
DASH = %x2D ; - dash
BAR = "|"
BARDASH = "|-"
BARPLUS = "|+"
GT = ">"
GTDASH = ">-"
GTPLUS = ">+"
DASHDASHDASH = "---"
DOTDOTDOT = "..."
```

### Numbers
```
octdigit = '0'..'7'
digit = '0'..'9'
hexdigit = '0'..'9' | 'a'..'f' | 'A'..'F'
int = '0' | ( '1'..'9' *digit )
frac = '.' *digit
ne_frac = '.' 1*digit
exp = ('e' | 'E')  [('-' | '+')] 1*digit
decimal_float_number = ['-']  ((int [frac] [exp] ) | (ne_frac [exp] ))
decimal_float_not_numbers = ".inf" | "-.inf" | ".NaN"

DECIMAL = (decimal_float_number | decimal_float_not_numbers)
HEX = ['-'] "0x" 1*hexdigit
OCTAL = ['-'] "0o" 1*octdigit
```
### Strings
```
letter = 'a'..'z' | 'A'..'Z'

alphanum = letter | digit
ident = letter *alphanum

json_unescaped =  0x20 .. 0x21 | 0x23 .. 0x5B | 0x5D .. 0x10FFFF
json_escaped = "\\" ( 0x22 | 0x5C | 0x2F | 0x62 | 0x66 | 0x6E | 0x72 | 0x74 | (0x75, 4hexdigit ) )
json_string_char = json_unescaped | json_escaped
JSONSTRING = "J" '"'  *json_string_char '"'

yamlscalar_char = Complement ('-' | '[' | ']' | '{' | '}' | '|' | '>' | ':' | ',' | '#' | '/' | '\\' | '"' | '\r' | '\n')
yamlscalar_startchar = Subtract (yamlscalar_char, (linews| '.' | '!' | '&' | '*'))
yamlscalar_endchar = Subtract (yamlscalar_char, linews)
YAMLSCALAR = yamlscalar_startchar [*yamlscalar_char yamlscalar_endchar]

yaml_basic_string_char = 0x9 | 0x20 .. 0x10ffff
yaml_unescaped_sqstring_char = Subtract((yaml_basic_string_char | '\n'), '\'')
YAMLSQSTRING = "'"  *(yaml_unescaped_sqstring_char | "''") "'"

yaml_basic_dqstring_char = Sub(yaml_basic_string_char, ('"' | '\\'))
yaml_dqstring_escaped_char = "\\"
                                 ( "0" (* ns-esc-null *)
                                 | "a" (* ns-esc-bell *)
                                 | "b" (* ns-esc-backspace *)
                                 | "t" | "\t" (* ns-esc-horizontal-tab *)
                                 | "n" (* ns-esc-line-feed *)
                                 | "v" (* ns-esc-vertical-tab *)
                                 | "f" (* ns-esc-form-feed *)
                                 | "r" (* ns-esc-carriage-return *)
                                 | "e" (* ns-esc-escape *)
                                 | ' ' (* ns-esc-space *)
                                 | '\"' (* ns-esc-double-quote *)
                                 | '/' (* ns-esc-slash *)
                                 | '\\' (* ns-esc-backslash *)
                                 | 'N' (* ns-esc-next-line *)
                                 | '_' (*ns-esc-non-breaking-space *)
                                 | "L" (* ns-esc-line-separator *)
                                 | "P" (* ns-esc-paragraph-separator *)
                                 | ( "x" 2hexdigit) (* ns-esc-8-bit *)
                                 | ( "u" 4hexdigit) (* ns-esc-16-bit *)
                                 | ( "U" 8hexdigit) (* ns-esc-32-bit *) )

yaml_dqstring_linebreak_1 = "\\" "\n" *(' '|'\t') ["\\"]
yaml_dqstring_linebreak_2 = "\n" *(' '|'\t')
yaml_dqstring_char = (yaml_basic_dqstring_char | yaml_dqstring_escaped_char )
YAMLDQSTRING = '"'  *(yaml_dqstring_char | yaml_dqstring_linebreak_1 | Plus(yaml_dqstring_linebreak_2)) '"'
```

Finally, there are C++ raw-string-literals:

```
RAWSTRING = "R" '"' [ident] "(" raw_string_content ")" [ident] '"'
raw_string_content = * any_char
```
with two constraints:

* the two idents above are identical (though perhaps both empty)
* the raw_string_content MUST NOT contain followed by the ident string

This constraint is complicated to express, and I refer the reader to
the C++ specification for more precision, but the intent should be
clear.

Note well that YAMLSQSTRING, YAMLDQSTRING, and RAWSTRING, can all
contain newlines.


### Comments

Note that Perl and C++ comments extend to the end of line or input (whichever comes first).
```
perl_comment = '#' *(Complement('\n'))
cpp_comment = "//" *(Complement('\n'))
c_comment = "/*" *(Complement( '*') | "*" Complement('/')) *'*' "*/"
comment = perl_comment | cpp_comment | c_comment
```

## Lexically Correct Text

The lexemes above cannot be used as-is (that is, handed as a group to
lex(1)) to construct a lexer, b/c they're ambiguous.  So we specify
how they are to be used to lexically process the text.  This implies a
state in the lexer over and above the buffer itself, but it's pretty
minimal.

a rawtoken is:
```
rawtoke = LBRACKET | LBRACE | RBRACKET |
	      RBRACE | COLON | COMMA | DASH |
		  BAR | BARDASH | BARPLUS |
	      GT | GTDASH | GTPLUS |
	      DASHDASHDASH |DOTDOTDOT |
	      DECIMAL | HEX | OCTAL |
	      JSONSTRING | YAMLSCALAR | YAMLSQSTRING | YAMLDQSTRING | RAWSTRING
```

A lexically correct text is:

```
[VERSIONSTRING] *(margin *(rawtoken|linews|comment) EOL) [margin *(rawtoken|linews|comment)]

```

This implies that the lexer must track whether it is at the beginning
of input, and the beginning of a line.  Otherwise, it merely lexes the
next `rawtoken` or `EOL` , skipping `linews` and `comment`s.

### Implementation of Lexing

The above description of lexing leads naturally to three separate lexers.

1. Lexer for beginning of input (`version_lexer`)


This lexer recognizes the `VERSIONSTRING` lexeme, returning the string
or failure.

2. Lexer for beginning of line (`margin_lexer`)

This lexer recognizes the `MARGIN` lexeme, returning success or
failure.

3. Lexer for rawtoken, comment, EOL (`token_lexer`)

This lexer recognizes all the tokens in `rawtoken`, as well as
`linews`, `comment` and `EOL`.  On `linews` or `comment`, it recurses.
On empty input, it returns `EOI', and otherwise it returns failure
(which signifies bad input).  It returns any token in the set of
`rawtoken`, as well as `EOL` and `EOI`.

## Producing a lexically correct stream of tokens

The module that performs this, has two state-variables (in addition to the lexer):

* `at_start` -- true if we're at the start of the text
* `at_bol` -- true if we're at the beginning of a line (that is, at
  the start of the text, or after an `EOL` as been read)

1. initially `at_start=true; at_bol=true`

2. `at_start=true`: use `version_lexer` and

  * set `at_start=false`
  * if it produces a token, then return that
  * else proceed to step 3.

3. `at_start=false; at_bol=true`: use `margin_lexer`, which always
   succeeds, then set `at_bol=false` and proceed to step 4.

4. `at_bol=false`: use `token_lexer`:

  * if it produces `EOL`, set `at_bol=true` and proceed to step 3
  * otherwise, return the token

This stream of tokens is the input to the next phase.

## Transducing a Lexically Correct Stream to the Final Lexemes

Once we have a lexically correct stream, we need to transduce it, to
add `INDENT` and `DEDENT` tokens.  There are two things going on here:

1. This phase has state variables for the current margin, saved
   margins, and the depth of nested "{}", "[]" seen so far.  It is
   convenient to use the OCaml types (should be straightforward to
   implement in other languages):
   
   ```
   type style_t = FLOW | BLOCK of int
   type style_stack_t = style_t list
   ```

	with initial value `style_stack = [BLOCK 0]`

2. matching `LBRACE`, `RBRACE` and `LBRACKET`, `RBRACKET`tokens are
   parenthesis-counted, and within them, no `INDENT`/`DEDENT`
   processing occurs.  tokens are passed-along.

   Concretely

   * when we see `LBRACE`/`LBRACKET`, we push `FLOW` on `style_stack`
   * when we see `RBRACE`/`RBRACKET`, we pop from `style_stack`,
     checking that the value popped is `FLOW' (otherwise we have a
     syntax error)


3. For all tokens not in the following subset, they are passed-along
   (though `LBRACE`, `LBRACKET` trigger the processing of part 2.

   ```
   YAMLSTRING YAMLSQSTRING YAMLDQSTRING RAWSTRING
   DECIMAL HEXADECIMAL OCTAL
   DASHDASHDASH DOTDOTDOT EOF
   DASH
   COLON
   ```
    In this step, we're going to compute a list of tokens we will return.

    * For all tokens except `DASH`, `COLON`, 
   
   
