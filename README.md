YAYAML Ain't YAML

This is an specification of a *slight* variant of
YAML, and an implementation of a parser (soon to be joined by printer) for that language.

The specification is an improvement in a number of ways, but I want to
be clear that this implementation does *not* meet the YAML 1.2 (or any
other) spec.  It is probably fair to say that it is as YAML-1.2
spec-compliant as any other YAML parser (other than the "reference
parsers", since there don't seem to exist any actually-usable YAML
parsers that are precisely specification-compliant.


# Motivation

YAML's goals are worthwhile, but they are not met by the current
specification and its implementations.

### Goals

1. YAML is easily readable by humans.
2. YAML data is portable between programming languages.
3. YAML matches the native data structures of agile languages.
4. YAML has a consistent model to support generic tools.
5. YAML supports one-pass processing.
6. YAML is expressive and extensible.
7. YAML is easy to implement and use.

### Goals not net

* Goal #2 is simply not met, for two reasons: 

	* YAML's data-model does not match that of most programming
	  languages: complex keys and anchors simply do not correspond to
	  PL data types in most cases.
  
	* The variation in both correctness and feature-support of
      different parsers effectively means that when you author a YAML
      document, you must be aware of which language and which parser
      will be used to parse it.  This ought to be unacceptable.
  
* I don't even know what Goal #3 might mean, but perhaps it means that
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

* Goal #5 is met, but superfluous: any YAML file *will* easily fit in
  memory, so one-pass processing is unnecessary.  Notwithstanding,
 aside from the workaround for Goal #3, I see no reason why one-pass
  processing is problematic to achieve in YAYAML.

* It's not clear what Goal #6 means: in what sense is YAML extensible,
  and in what sense *should* it be extensible?
  
* Goal #7 has not been met, in the sense that many YAML users find
  that they need to know precisely which YAML parser they'll be using,
  in order to know how to write their YAML.  They practice "defensive
  YAML", and this is a *prima facie* proof that this goal has not been
  met.



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
