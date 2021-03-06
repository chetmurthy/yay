# YAY Ain't YAML

This is an specification of a *slight* variant of
YAML, and an implementation of a parser (soon to be joined by printer) for that language.

The specification is an improvement in a number of ways, but I want to
be clear that this implementation does *not* meet the YAML 1.2 (or any
other) spec.  It is probably fair to say that it is as YAML-1.2
spec-compliant as any other YAML parser (other than the "reference
parsers", since there don't seem to exist any actually-usable YAML
parsers that are precisely specification-compliant.

The YAY specification can be found [here](YAY-SPECIFICATION.md).

## This Implementation

To cut to the chase, this project is an implementation of a reference
YAY parser and printer in OCaml.  The easiest way to install is via
opam:

```
opam install .
```

This will ensure that pre-reqs are installed.  There are copious
examples, which can be found by looking at files containing the string
"test".  In addition, this implementation passes both the JSON
Testsuite and (a suitably modified) YAML Testsute.  Both are included
as Git subprojects.

Further instructions for installation can be found [here](INSTALL.md).

## Motivation

YAML's goals are worthwhile, but they are not met by the current
specification and its implementations.

### YAML's Goals

1. YAML is easily readable by humans.
2. YAML data is portable between programming languages.
3. YAML matches the native data structures of agile languages.
4. YAML has a consistent model to support generic tools.
5. YAML supports one-pass processing.
6. YAML is expressive and extensible.
7. YAML is easy to implement and use.

### Why YAML does not meet these Goals

* "2. YAML data is portable between programming languages."

  Two reasons:

	* YAML's data-model does not match that of most programming
	  languages: complex keys and anchors simply do not correspond to
	  PL data types in most cases.
  
	* The variation in both correctness and feature-support of
      different parsers effectively means that when you author a YAML
      document, you must be aware of which language and which parser
      will be used to parse it.  This ought to be unacceptable.
  
* "4. YAML has a consistent model to support generic tools."

  I don't even know what this might mean, but perhaps it means that
  YAML supports tooling via "directives" that give instructions to the
  tooling, separate from the documents themselves.  This might be true
  and useful, but it seems pretty niche, and I know of no examples of
  the use of directives for this sort of thing.

  Also, one could solve the problem of directives by appending a
  document to the end of the YAML file (the idea that a YAML file is
  too large to load into memory before parsing seems pretty
  far-fetched -- it's human-readable and -writable, after all) and put
  directives there.  This specification allows to scan the YAML file
  at a level that is nearly-lexical, to demarcate documents without
  parsing, so it is feasible to find the last document in a YAML file,
  parse it, and then back up to parse the entire file.

* "5. YAML supports one-pass processing."

  Met, but superfluous: any YAML file *will* easily fit in memory, so
  one-pass processing is unnecessary.  Notwithstanding, aside from the
  workaround for Goal #3, I see no reason why one-pass processing is
  problematic to achieve in YAY.

* "6. YAML is expressive and extensible."

  It's not clear what this means: in what sense is YAML extensible,
  and in what sense *should* it be extensible?
  
* "7. YAML is easy to implement and use."

  Has not been met, in the sense that many YAML users find that they
  need to know precisely which YAML parser they'll be using, in order
  to know how to write their YAML.  They practice "defensive YAML",
  and this is a *prima facie* proof that this goal has not been met.

## YAY's Design Goals

YAY's specification is structured in the manner of typical language
specs: it specifies the character-set of the language, then the
lexemes, and then the grammar.  The goals of YAY (in addition to
those of YAML) are:

1. enable and deliver straightforward implementation using lex/yacc
tooling, and hence to ensure that all implementations can have
identical behaviour (which documents they accept/reject, and in-memory
semantics of those documents).

2. YAY is merely (*merely*) a nicer syntax for writing JSON
   documents.  Every YAY document can be thought of as a JSON
   document, and JSON documents are accepted as YAY "flow style".
  
   The user can firmly rely on YAY as being merely (*merely*) a
   convenient syntax for writing JSON documents.  There are a few
   deviations from this rule, and they will be explained in this
   specification.  Those deviations concern only the first line, and
   multi-document support: it will always be the case that a single
   JSON document in a file, is accepted as a YAY document.
   



================================================================

Lexemes

'{' '}' '[' ']' ':' ','
'"' [^ '"'] '"'
false
null
true

<number>

Added for block mode

'-'
'>'
'|'
'---'
'...'
'#'

<raw-strings>

Forbidden, b/c block mode

'--'
'..'

================================================================

Plan

(1) lowest-layer lexer will recognize tokens, and also newlines and
"possibly-empty leading spaces".

(2) block-mode wrapper will track current indent-or-flow-style

When in flow-style, indent/dedent do not get passed-thru, nor do
newlines.

================================================================

Parsing plan

(1) in block mode, the start of a dict-key, or of an array-entry, sets
an indent-stop.  So

a: b: c
   d: e

is valid.

(2) also, a scalar value sets an indent-stop.  So

a: b c d
   e f g

is a single dict-entry, with value "b c d e f g" (b/c of folding)

and is equal to

a:
 b c d
 e f g

## License

YAY is currently distributed under the Apache 2.0 license.  I'm happy to redistribute
under other OSS licenses, that helps.
