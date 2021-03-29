
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
