# YAYAML Specification

This specification is for version 0.1.

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
