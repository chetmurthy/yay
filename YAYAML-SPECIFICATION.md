# YAYAML Specification

This specification is for version 0.1.

Parts of this specification have been copied directly from
[the JSON specification](https://tools.ietf.org/html/rfc7159).

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

In addition, YAYAML has a set of special characters, that cannot
appear in unquoted YAML scalars: when one wants to use those, there
are always C++ raw-string-literals.

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

### YAYAML Version Line

YAYAML texts are expressed an optional version-line, followed by a
stream of documents.

If present, the version-line MUST begin at the first character of the
text, and consists in:

### White Space

There are three kinds of white space:
```
eol = "\r" ? "\n"
marginws = ' '*
linews = [' ' '\t']
```

```1
yayaml-version = "YAML-" digit+ "." digit+ eol
```
The rest of the lexemes are:

special characters:
```
begin-array     = %x5B  ; [ left square bracket
begin-object    = %x7B  ; { left curly bracket
end-array       = %x5D  ; ] right square bracket
end-object      = %x7D  ; } right curly bracket
name-separator  = %x3A  ; : colon
value-separator = %x2C  ; , comma
```




