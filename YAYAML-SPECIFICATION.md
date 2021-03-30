# YAYAML Specification

This specification is for version 0.1.

Parts of this specification have been copied directly from
[the JSON specification](https://tools.ietf.org/html/rfc7159).

Other parts have been copied directly from
[the YAML specification](https://yaml.org/spec/1.2/spec.html).

# Goals of YAYAML

YAYAML starts off with the same goals as YAML:

1. YAYAML is easily readable by humans.
2. YAYAML data is portable between programming languages.
3. YAYAML matches the native data structures of agile languages.
4. YAYAML has a consistent model to support generic tools.
5. YAYAML supports one-pass processing.
6. YAYAML is expressive and extensible.
7. YAYAML is easy to implement and use.

But adds:

8. YAYAML is specified in terms of familiar lex/yacc concepts, and
   straightforwardly implementable using those tools.
   
9. YAYAML facilitates perfect specification-compliance from all
   implementations.

10. YAYAML is *merely* a nicer syntax for JSON.

    In all sense that matter, YAYAML users can assume that each YAYAML
    file is equivalent to a JSON file, and YAYAML implementations can
    produce that JSON file on-demand.  In addition, every JSON file is
    already a YAYAML file.

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
A "document" is a stream of characters that express a single YAYAML
value.  So a YAYAML text MAY contain one or more YAYAML documents.  A
YAYAML text MUST contain at least one YAYAML document.

# High-Level Description of YAYAML

For those who are unfamiliar with YAML,
[this](https://yaml.org/spec/1.2/spec.html) is the specification, and
[here](https://www.cloudbees.com/blog/yaml-tutorial-everything-you-need-get-started/)
is a YAML tutorial.  There are many such tutorials, and I'm not
recommending this one; just providing an example.

YAYAML can be described as YAML with the following additions:

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

In addition, the special characters used to express YAYAML structure
(viz. "[", "]", "-", etc) cannot appear in unquoted YAML scalars: when
one wants to use those, there are always C++ raw-string-literals.

# Syntactic Specification

The syntax of YAYAML is specified in terms of character set, lexemes,
and a grammar.

## Character Set

The YAYAML character-set is Unicode.  Implementations MUST accept
UTF-8, and MAY convert other files in other codepages to UTF-8 (rather
than reject).  This specfication does NOT discuss how other codepages
are represented in storage, and it is assumed that those are "transfer
encoding" issue.

## Lexemes

In the following, uppercase defintions (e.g. "INDENT") will be used
later in the specificadtion: they correspond to the "tokens"; the
other definitions are auxiliary.

### White Space

There are three kinds of white space:
```
EOL = "\r" ? "\n"
INDENT = ' '*
LINEWS = [' ' '\t']
```

### YAYAML Version Line

YAYAML texts are expressed an optional version-line, followed by a
stream of documents.

If present, the version-line MUST begin at the first character of the
text, and consists in:

```
YAMLVERSION = "YAML-" digit+ "." digit+ eol
```
### The tokens

The rest of the lexemes are:

#### Special Characters

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

#### Numbers
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
#### Strings
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
YAMLDQSTRING = "\""  *(yaml_dqstring_char | yaml_dqstring_linebreak_1 | Plus(yaml_dqstring_linebreak_2)) '"'
```
