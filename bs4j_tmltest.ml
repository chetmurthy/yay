open OUnit2
open OUnitTest

let tml_dir = "/home/chet/Hack/Github/yaml/yaml-test-suite/test"

let only_tags = []
let exclude_tags = ["anchor"; "alias"; "explicit-key"; "tag"; "complex-key"; "empty-key"]

let skiplist = [
  ("5U3A.tml", Some "supported block syntax in BS4J")
; ("A2M4.tml", Some "``?'' used for key, unsupported syntax")
; ("9C9N.tml", Some "supported flow syntax in BS4J")
; ("87E4.tml", Some "flow style should be JSON, yaml quotations")
; ("U9NS.tml", Some "``:'' is a special char, cannot be used in raw scalars")
; ("MYW6.tml", Some "raw-string-literals obviate this")
; ("J3BT.tml", Some "raw-string-literal, yaml quotation")
; ("753E.tml", Some "raw-string-literals obviate this")
; ("6H3V.tml", Some "yaml quotations")
; ("Q5MG.tml", Some "<TAB> at margin")
; ("9DXL.tml", Some "multiple docs must all be prefixed by ``---''")
; ("2LFX.tml", Some "directives are unsupported")
; ("BEC7.tml", Some "change filetype to BS4J; filetype matching is strict")
; ("CC74.tml", Some "tag directives are unsupported")
; ("9WXW.tml", Some "(tag) directives are unsupported")
; ("27NA.tml", Some "content on same line as ``---'' is supported in BS4J")
; ("6WLZ.tml", Some "(tag) directives are unsupported")
; ("6LVF.tml", Some "directives are unsupported")
; ("P2AD.tml", Some "neither indentation nor chomping are necessary")
; ("6ZKB.tml", Some "multiple docs must all be prefixed with ``---''")
; ("PUW8.tml", Some "docs cannot be null")
; ("6XDY.tml", Some "docs cannot be null")
; ("RZT7.tml", Some "``:'' is a special char, cannot be used in raw scalars")
; ("RTP8.tml", Some "change filetype to BS4J; filetype matching is strict")
; ("UT92.tml", Some "yaml quotations, also multiline scalars forbidden in flow style")
; ("W4TN.tml", Some "change filetype to BS4J; filetype matching is strict")
; ("F8F9.tml", Some "chomping/stripping is unnecessary")
; ("A984.tml", Some "margin-indent must be identical for multiline yamlscalars")
; ("UDM2.tml", Some "flow scalar with special chars must be quoted")
; ("T26H.tml", Some "whitespace precision is best done with raw-string-literals")
; ("S7BG.tml", Some "flow scalar with special chars must be quoted")
; ("FBC9.tml", Some "special chars in scalars require quotations")
; ("4ZYM.tml", Some "yaml quotations")
; ("LQZ7.tml", Some "yaml quotations")
; ("3MYT.tml", Some "special chars in scalars require quotations")
; ("DWX9.tml", Some "whitespace precision is best done with raw-string-literals")
; ("G992.tml", Some "whitespace precision is best done with raw-string-literals")
; ("4QFQ.tml", Some "whitespace precision is best done with raw-string-literals")
; ("4Q9F.tml", Some "whitespace precision is best done with raw-string-literals")
; ("36F6.tml", Some "margin-indent must be identical for multiline yamlscalars")
; ("M29M.tml", Some "whitespace precision is best done with raw-string-literals")
; ("MJS9.tml", Some "whitespace precision is best done with raw-string-literals")
; ("Q8AD.tml", Some "yaml quotations")
; ("H2RW.tml", Some "whitespace precision is best done with raw-string-literals")
; ("T5N4.tml", Some "whitespace precision is best done with raw-string-literals")
; ("HS5T.tml", Some "whitespace precision is best done with raw-string-literals")
; ("5T43.tml", Some "yaml quotations")
; ("B3HG.tml", Some "whitespace precision is best done with raw-string-literals")
; ("AB8U.tml", Some "whitespace precision is best done with raw-string-literals")
; ("DBG4.tml", Some "special chars in scalars require quotations")
; ("93WF.tml", Some "whitespace precision is best done with raw-string-literals")
; ("EX5H.tml", Some "whitespace precision is best done with raw-string-literals")
; ("DK3J.tml", Some "special chars in scalars require quotations")
; ("7T8X.tml", Some "whitespace precision is best done with raw-string-literals")
; ("6FWR.tml", Some "whitespace precision is best done with raw-string-literals")
; ("FP8R.tml", Some "whitespace precision is best done with raw-string-literals")
; ("2EBW.tml", Some "special chars in scalars require quotations")
; ("82AN.tml", Some "special chars in scalars require quotations")
; ("8CWC.tml", Some "special chars in scalars require quotations")
; ("96L6.tml", Some "whitespace precision is best done with raw-string-literals")
; ("M9B4.tml", Some "whitespace precision is best done with raw-string-literals")
; ("R4YG.tml", Some "whitespace precision is best done with raw-string-literals")
; ("8G76.tml", Some "empty documents aren't supported")
; ("4V8U.tml", Some "special chars in scalars require quotations")
; ("K527.tml", Some "whitespace precision is best done with raw-string-literals")
; ("5BVJ.tml", Some "whitespace precision is best done with raw-string-literals")
; ("6VJK.tml", Some "whitespace precision is best done with raw-string-literals")
; ("TS54.tml", Some "whitespace precision is best done with raw-string-literals")
; ("XV9V.tml", Some "yaml quotations")
; ("6JQW.tml", Some "whitespace precision is best done with raw-string-literals")
; ("EXG3.tml", Some "special chars in scalars require quotations")
; ("9YRD.tml", Some "whitespace precision is best done with raw-string-literals")
; ("A6F9.tml", Some "special chars in scalars require quotations")
; ("G4RS.tml", Some "yaml quotations")
; ("NB6Z.tml", Some "special chars in scalars require quotations")
; ("QF4Y.tml", Some "flow style should be JSON, yaml quotations")
; ("8UDB.tml", Some "yaml quotations, also multiline scalars forbidden in flow style")
; ("C2DT.tml", Some "empty value forbidden")
; ("9BXH.tml", Some "empty value forbidden")
; ("G5U8.tml", Some "special chars in scalars require quotations")
; ("9MMW.tml", Some "complex key unsupported")
; ("L9U5.tml", Some "flow style should be JSON, trailing comma")
; ("NJ66.tml", Some "yaml quotations, also multiline scalars forbidden in flow style")
; ("5C5M.tml", Some "trailing comma in flow style")
; ("6HB6.tml", Some "whitespace precision is best done with raw-string-literals")
; ("7ZZ5.tml", Some "subsidiary values MUST be indented")
; ("4ABK.tml", Some "special chars in scalars require quotations, empty values, trailing comma")
; ("8KB6.tml", Some "multi line scalars in flow keys require quotations, empty values")
; ("F6MC.tml", Some "whitespace precision is best done with raw-string-literals")
; ("AZ63.tml", Some "subsidiary values MUST be indented")
; ("RLU9.tml", Some "subsidiary values MUST be indented")
; ("D83L.tml", Some "explicit indent is superfluous")
; ("AVM7.tml", Some "empty documents aren't supported")
; ("HWV9.tml", Some "empty documents aren't supported")
; ("QT73.tml", Some "empty documents aren't supported")
; ("S4T7.tml", Some "doc end-delimiter can only come after start-delimiter")
; ("K858.tml", Some "empty scalar values are unsupported")
; ("HMK4.tml", Some "whitespace precision is best done with raw-string-literals")
; ("M7A3.tml", Some "doc end-delimiter can only come after start-delimiter")
; ("W42U.tml", Some "empty value")
; ("AZW3.tml", Some "special chars in scalars require quotations, empty values")
; ("7Z25.tml", Some "trailing text after document")
; ("98YD.tml", Some "empty document with comments")
; ("QB6E.tml", Some "multiline quoted-strings don't need to obey indentation rules: continuation lines can have arbitray indent ")
; ("RXY3.tml", Some  "multiline quoted-strings don't need to obey indentation rules: continuation lines can have arbitray indent ")
; ("5TRB.tml", Some  "multiline quoted-strings don't need to obey indentation rules: continuation lines can have arbitrary indent")
; ("SU5Z.tml", Some "comments are allowed anywhere")
; ("BF9H.tml", Some "comments are allowed anywhere")
; ("X4QW.tml", Some "comments are allowed anywhere")
; ("W9L4.tml", Some "indentation in empty lines is superfluous outside of quotes")
; ("EB22.tml", Some "with multiple documents, only headers are mandatory")
; ("9JBA.tml", Some "comments are allowed anywhere")
; ("9KBC.tml", Some "value can start on same line as header")
; ("ZL4Z.tml", Some "mappings, arrays can nest in arbitrary order")
; ("CVW2.tml", Some "comments are allowed anywhere")
; ("ZCZ6.tml", Some "mappings, arrays can nest in arbitrary order")
; ("5LLU.tml", Some "indentation in empty lines is superfluous outside of quotes")
; ("D49Q.tml", Some  "multiline quoted-strings are allowed anywhere quoted strings are")
; ("7LBH.tml", Some  "multiline quoted-strings are allowed anywhere quoted strings are")
; ("BS4K.tml", Some "comments are allowed anywhere")
]

let select_tests ?(exclude_tags=[]) ?(only_tags=[]) l =
  let open Tml in
  let l =
    if only_tags = [] then l else
      l |> List.filter (fun t ->
          let tags = Tml.tags t in
          tags |> List.exists (fun s -> List.mem s only_tags)) in
  if exclude_tags = [] then l else
    l |> List.filter (fun t ->
        let tags = Tml.tags t in
        tags |> List.for_all (fun s -> not(List.mem s exclude_tags)))

let make_test t =
  let open Tml in
  let base = Fpath.(t.filename |> v |> basename) in
  let tagsl = Tml.tags t in
  match List.assoc base skiplist with
    msg ->
    let msg = match msg with None -> t.name | Some s -> s in
    let name = Fmt.(str "%s (%s) [%a]" t.name base (list ~sep:(const string " ") string) tagsl) in
    base >::: [
      name >:: (fun ctxt ->
          Tml.warning (Fmt.str "%s: Not handled: %s" t.filename msg)
        )
    ]
  | exception Not_found ->
    base >::: [
      let name = Fmt.(str "%s (%s) [%a]" t.name base (list ~sep:(const string " ") string) tagsl) in
      name >:: (fun ctxt ->
          BS4J.exec t
        )
    ]

let parse1 n =
  let file =
    if n |>  Fpath.v |> Bos.OS.File.exists |> Rresult.R.get_ok then n
    else Fpath.(to_string (append (v tml_dir) (v n))) in
  Tml.(BS4J.parse_yaml (from_file file))

let exec1 n =
  let file =
    if n |>  Fpath.v |> Bos.OS.File.exists |> Rresult.R.get_ok then n
    else Fpath.(to_string (append (v tml_dir) (v n))) in
  Tml.(BS4J.exec (from_file file))

let tests = "BS4J testsuite" >::: (
    let fl = Tml.files ~override_dir:"bs4j-overrides" tml_dir in
    let tests = List.map Tml.from_file fl in
    let tests = select_tests ~only_tags ~exclude_tags tests in
    List.map make_test tests
  )

if not !Sys.interactive then
  run_test_tt_main tests
;;
