open OUnit2
open OUnitTest
open Pa_ppx_testutils

open Yaytypes
open Yaylexing
open Yaypostlexing

Pa_ppx_base.Pp_MLast.Ploc.pp_loc_verbose := true ;;

let warning s = Fmt.(pf stderr "%s\n%!" s)

let matches ~pattern text =
  match Str.search_forward (Str.regexp (Str.quote pattern)) text 0 with
    _ -> true
  | exception Not_found -> false

let assert_raises_exn_pattern pattern f =
  Testutil.assert_raises_exn_pred
    (function
        Failure msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Stdlib.Stream.Error msg) when matches ~pattern msg -> true
      | Stdlib.Stream.Error msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Failure msg) when matches ~pattern msg -> true
      | Invalid_argument msg when matches ~pattern msg -> true
      | _ -> false
    )
    f

type toks = token list [@@deriving show { with_path = false},eq]

let printer = show_toks

let tokens_of_string s =
  List.map fst (Final.ocamllex_string s)

let lexing = "lexing" >::: [
    "simple" >:: (fun ctxt ->
        assert_equal ~printer
          [YAMLSTRING "a";
           EOF]
          (tokens_of_string {|a|})
      )
  ; "2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; (INDENT (0, 1));
           (INDENT (1, 2)); (YAMLSTRING "null");
           (DEDENT (1, 2)); (DEDENT (0, 1)); EOF]
          (tokens_of_string "\na:\n  null")
      )
  ; "3" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; (INDENT (0, 1));
           (INDENT (1, 3)); (YAMLSTRING "b");
           (DEDENT (1, 3)); (DEDENT (0, 1));
           (YAMLSTRING "c"); COLON; (INDENT (0, 1));
           (INDENT (1, 3)); (YAMLSTRING "d");
           (DEDENT (1, 3)); (DEDENT (0, 1)); EOF]
          (tokens_of_string {|
a: b
c: d
|})
      )
  ; "flow" >:: (fun ctxt ->
        assert_equal ~printer
          [LBRACKET; (YAMLDQSTRING "\"a\""); COMMA;
           (YAMLDQSTRING "\"b\""); RBRACKET;
           EOF]
          (tokens_of_string {|["a", "b"]|})
      )
  ; "flow-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; (INDENT (0, 1));
           LBRACKET; (YAMLDQSTRING "\"a\""); COMMA;
           (YAMLDQSTRING "\"b\""); RBRACKET; (DEDENT (0, 1));
           EOF]
          (tokens_of_string {|
a:
 ["a", "b"]
|})
      )
  ; "flow-2'" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; (INDENT (0, 1));
           LBRACKET; (YAMLDQSTRING "\"a\""); COMMA;
           (YAMLDQSTRING "\"b\""); COMMA; RBRACKET; (DEDENT (0, 1));
           EOF]
          (tokens_of_string {|
a:
 ["a", "b",]
|})
      )
  ; "flow-3" >:: (fun ctxt ->
        assert_equal ~printer
          [LBRACE; (YAMLSTRING "hr");
           COLON; (DECIMAL "63"); RBRACE;
           EOF]
          (tokens_of_string {|
{ hr: 63
}
|})
      )
  ; "indents" >:: (fun ctxt ->
      assert_equal ~printer
        [(YAMLSTRING "a"); COLON; (INDENT (0, 1)); (YAMLSTRING "b"); COLON;
         (INDENT (1, 2)); (INDENT (2, 4)); (YAMLSTRING "c"); (DEDENT (2, 4));
         (DEDENT (1, 2)); (DEDENT (0, 1)); EOF]
        (tokens_of_string {|
a:
 b: c
|})
    )
  ; "indents-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; (INDENT (0, 1)); (YAMLSTRING "b"); COLON;
           (INDENT (1, 2)); (INDENT (2, 4)); (YAMLSTRING "c"); (DEDENT (2, 4));
           (DEDENT (1, 2)); (YAMLSTRING "d"); COLON; (INDENT (1, 2)); (INDENT (2, 4));
           (YAMLSTRING "e"); (DEDENT (2, 4)); (DEDENT (1, 2)); (DEDENT (0, 1)); 
           EOF]
          (tokens_of_string {|
a:
 b: c
 d: e
|})
      )
  ; "indents-3" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; (INDENT (0, 1)); DASH; (INDENT (1, 2));
           (INDENT (2, 3)); (YAMLSTRING "b"); (DEDENT (2, 3)); (DEDENT (1, 2)); 
           DASH; (INDENT (1, 3)); (YAMLSTRING "d"); (DEDENT (1, 3)); (DEDENT (0, 1));
           EOF]
          (tokens_of_string {|
a:
 - b
 - d
|})
      )
  ; "rawstring" >:: (fun ctxt ->
        assert_equal ~printer
          [RAWSTRING {|R"a(foo)a"|};
           EOF]
          (tokens_of_string {|
R"a(foo)a"
|})
      )
  ; "rawstring-fold" >:: (fun ctxt ->
        assert_equal ~printer
          [GT; (INDENT (0, 1)); (RAWSTRING "R\"a(foo)a\""); (DEDENT (0, 1)); EOF]
          (tokens_of_string {|
>R"a(foo)a"
|})
      )
  ; "rawstring-lit" >:: (fun ctxt ->
        assert_equal ~printer
          [BAR; (INDENT (0, 1)); (RAWSTRING "R\"a(foo)a\""); (DEDENT (0, 1)); EOF]
          (tokens_of_string {|
|R"a(foo)a"
|})
      )
  ; "rawstring-fold-chomp" >:: (fun ctxt ->
        assert_equal ~printer
          [GTDASH; (INDENT (0, 2)); (RAWSTRING "R\"a(foo)a\""); (DEDENT (0, 2)); EOF]
          (tokens_of_string {|
>-R"a(foo)a"
|})
      )
  ; "rawstring-lit-chomp" >:: (fun ctxt ->
        assert_equal ~printer
          [BARDASH; (INDENT (0, 2)); (RAWSTRING "R\"a(foo)a\""); (DEDENT (0, 2)); EOF]
          (tokens_of_string {|
|-R"a(foo)a"
|})
      )
  ; "rawstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 2)); RAWSTRING {|R"(foo)"|};
           (DEDENT (0, 2)); EOF]
          (tokens_of_string {|
  R"(foo)"
|})
      )
  ; "rawstring-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 2)); RAWSTRING {|R"((foo))"|};
           (DEDENT (0, 2)); EOF]
          (tokens_of_string {|
  R"((foo))"
|})
      )
  ; "strings-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; (INDENT (0, 1)); (INDENT (1, 2)); (YAMLSTRING "b");
           (YAMLSTRING "c"); (DEDENT (1, 2)); (DEDENT (0, 1)); EOF]
          (tokens_of_string {|
a:
  b
  c
|})
      )
  ; "empty-1" >:: (fun ctxt ->
        assert_equal ~printer
          [EOF]
          (tokens_of_string {||})
      )
  ; "empty-2" >:: (fun ctxt ->
        assert_equal ~printer
          [DASHDASHDASH; DOTDOTDOT;
           EOF]
          (tokens_of_string {|
---
...
|})
      )
  ; "comment-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a");
           EOF]
          (tokens_of_string {|
# foo
a
|})
      )
  ; "comment-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a");
           EOF]
          (tokens_of_string {|
// foo
a
|})
      )
  ; "comment-3" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a");
           EOF]
          (tokens_of_string {|
/* foo * / ***/
a
|})
      )
  ; "float-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "hr"); COLON; (INDENT (0, 1)); (INDENT (1, 5)); (DECIMAL "65");
           (DEDENT (1, 5)); (DEDENT (0, 1)); EOF]
          (tokens_of_string {|
hr:  65    # Home runs
|})
      )
  ; "fold-1" >:: (fun ctxt ->
        assert_equal ~printer
          [DASHDASHDASH; GT; (INDENT (0, 2));
           (RAWSTRING
              "R\"(Mark McGwire's\n     year was crippled\n     by a knee injury.)\"");
           (DEDENT (0, 2)); EOF]
          (tokens_of_string {|--- >
  R"(Mark McGwire's
     year was crippled
     by a knee injury.)"|})
      )
  ; "yamlstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); COLON; (INDENT (0, 1)); (INDENT (1, 3));
           (YAMLSTRING "b c"); (DEDENT (1, 3)); (DEDENT (0, 1)); EOF]
          (tokens_of_string {|a: b c|})
      )
  ; "yamlstring-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "a"); (YAMLSTRING "b"); EOF]
          (tokens_of_string {|
a
b
|})
      )
  ; "yamlstring-3" >:: (fun ctxt ->
        assert_equal ~printer
          [(INDENT (0, 2)); (YAMLSTRING "a"); (YAMLSTRING "b"); (YAMLSTRING "c"); (DEDENT (0, 2)); EOF]
          (tokens_of_string {|
  a
  b
  c
|})
      )

  ; "yamlstring-2-fold" >:: (fun ctxt ->
        assert_equal ~printer
          [GT; (INDENT (0, 1)); (YAMLSTRING "a"); (YAMLSTRING "b"); (YAMLSTRING "c");
           (DEDENT (0, 1)); EOF]
          (tokens_of_string {|
>
 a
 b
 c
|})
      )
  ; "yamlstring-3-fold" >:: (fun ctxt ->
        assert_equal ~printer
          [GTDASH; (INDENT (0, 1)); (YAMLSTRING "a"); (YAMLSTRING "b");
           (YAMLSTRING "c"); (DEDENT (0, 1)); EOF]
          (tokens_of_string {|
>-
 a
 b
 c
|})
      )



  ; "dqstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "unicode"); COLON; (INDENT (0, 1)); (INDENT (1, 9));
           (YAMLDQSTRING "\"Sosa did fine.\\u263A\""); (DEDENT (1, 9));
           (DEDENT (0, 1)); EOF]
          (tokens_of_string {|unicode: "Sosa did fine.\u263A"|})
      )
  ; "dqstring-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "control"); COLON; (INDENT (0, 1)); (INDENT (1, 9));
           (YAMLDQSTRING "\"\\b1998\\t1999\\t2000\\n\""); (DEDENT (1, 9));
           (DEDENT (0, 1)); EOF]
          (tokens_of_string {|control: "\b1998\t1999\t2000\n"|})
      )
  ; "dqstring-3" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "hex esc"); COLON; (INDENT (0, 1)); (INDENT (1, 9));
           (YAMLDQSTRING "\"\\x0d\\x0a is \\r\\n\""); (DEDENT (1, 9));
           (DEDENT (0, 1)); EOF]
          (tokens_of_string {|hex esc: "\x0d\x0a is \r\n"|})
      )
  ; "sqstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "single"); COLON; (INDENT (0, 1)); (INDENT (1, 8));
           (YAMLSQSTRING "'\"Howdy!\" he cried.'"); (DEDENT (1, 8)); (DEDENT (0, 1));
           EOF]
          (tokens_of_string {|single: '"Howdy!" he cried.'|})
      )
  ; "sqstring-2" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSTRING "quoted"); COLON; (INDENT (0, 1)); (INDENT (1, 8));
           (YAMLSQSTRING "' # Not a ''comment''.'"); (DEDENT (1, 8));
           (DEDENT (0, 1)); EOF]
          (tokens_of_string {|quoted: ' # Not a ''comment''.'|})
      )
  ; "sqstring-3" >:: (fun ctxt ->
        assert_equal ~printer
          [(YAMLSQSTRING "'tie-fighter'"); COLON; (INDENT (0, 1)); (INDENT (1, 15));
           (YAMLSQSTRING "'|\\-*-/|'"); (DEDENT (1, 15)); (DEDENT (0, 1)); EOF]
          (tokens_of_string {|'tie-fighter': '|\-*-/|'|})
      )
  ]

open Yayparse0

let printer = Yaytypes.show_yaml
let docs_printer = Yaytypes.show_yaml_list
let cmp = Yaytypes.equal_yaml
let docs_cmp = Yaytypes.equal_yaml_list

let of_string_exn s = s |> parse_string parse_doc_eoi |> Yaytypes.json2yaml
let docs_of_string_exn s = s |> parse_string parse_docs_eoi |> List.map Yaytypes.json2yaml

let parsing = "parsing" >::: [
    "simple" >:: (fun ctxt ->
        assert_equal ~printer
          (`String"a")
          (of_string_exn {|a|})
      )
  ; "2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `Null)]))
          (of_string_exn "\na:\n  null")
      )

  ; "3" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `String ("b")); ("c", `String ("d"))]))
          (of_string_exn {|
a: b
c: d
|})
      )
  ; "flow" >:: (fun ctxt ->
        assert_equal ~printer
          (`A ([`String ("a"); `String ("b")]))
          (of_string_exn {|["a", "b"]|})
      )
  ; "flow-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `A ([`String ("a"); `String ("b")]))]))
          (of_string_exn {|
a:
 ["a", "b"]
|})
      )
  ; "flow-2'" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `A ([`String ("a"); `String ("b")]))]))
          (of_string_exn {|
a:
 ["a", "b",]
|})
      )
  ; "flow-3" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `A ([`A ([`String ("a"); `String ("b")])]))]))
          (of_string_exn {|
a:
 [["a", "b",],]
|})
      )
  ; "flow-4" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `A ([`O ([("a", `String ("b"))])]))]))
          (of_string_exn {|
a:
 [{"a": "b",},]
|})
      )
  ; "flow-5" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `O ([("e", `O ([("a", `String ("b"))]))]))]))
          (of_string_exn {|
a:
 { e: {"a": "b",}, }
|})
      )
  ; "indents" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `O ([("b", `String ("c"))]))]))
          (of_string_exn {|
a:
 b: c
|})
      )
  ; "indents-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `O ([("b", `String ("c")); ("d", `String ("e"))]))]))
          (of_string_exn {|
a:
 b: c
 d: e
|})
      )
  ; "indents-3" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `A ([`String ("b"); `String ("d")]))]))
          (of_string_exn {|
a:
 - b
 - d
|})
      )
  ; "rawstring" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("foo"))
          (of_string_exn {|
R"a(foo)a"
|})
      )
  ; "rawstring-fold" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("foo bar\n"))
          (of_string_exn {|
>R"a(foo
     bar
     )a"
|})
      )
  ; "rawstring-lit" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("foo\nbar\n"))
          (of_string_exn {|
|R"a(foo
     bar
     )a"
|})
      )
  ; "rawstring-fold-chomp" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("foo bar"))
          (of_string_exn {|
>-R"a(foo
      bar
      )a"
|})
      )
  ; "rawstring-lit-chomp" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("foo\nbar"))
          (of_string_exn {|
|-R"a(foo
      bar
      )a"
|})
      )
  ; "rawstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("foo"))
          (of_string_exn {|
  R"(foo)"
|})
      )
  ; "strings-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `String ("b c"))]))
          (of_string_exn {|
a:
  b
  c
|})
      )
  ; "comment-1" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a"))
          (of_string_exn {|
# foo
a
|})
    )
  ; "comment-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a"))
          (of_string_exn {|
// foo
a
|})
    )
  ; "comment-3" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a"))
          (of_string_exn {|
/* foo * / ***/
a
|})
    )
  ; "2.1" >:: (fun ctxt ->
        assert_equal ~printer
          (`A (
              [`String ("Mark McGwire")
              ; `String ("Sammy Sosa")
              ; `String ("Ken Griffey")]
            ))
          (of_string_exn {|- Mark McGwire
- Sammy Sosa
- Ken Griffey|})
      )
  ; "float-1" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("hr", `Float (65.)); ("avg", `Float (0.288))]))
          (of_string_exn {|
hr:  65    # Home runs
avg: 0.288
|})
      )
  ; "yamlstring-1" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("a", `String ("b c"))]))
          (of_string_exn {|a: b c|})
      )

  ; "yamlstring-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a b"))
          (of_string_exn {|
a
b
|})
      )
  ; "yamlstring-3" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a b c"))
          (of_string_exn {|
  a
  b
  c
|})
      )
  ; "yamlstring-3-fold" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a b c\n"))
          (of_string_exn {|
>
 a
 b
 c
|})
      )
  ; "yamlstring-3-fold-chomp" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a b c"))
          (of_string_exn {|
>-
 a
 b
 c
|})
      )
  ; "yamlstring-3-fold-add" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a b c\n"))
          (of_string_exn {|
>+
 a
 b
 c
|})
      )
  ; "yamlstring-3-literal" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a\nb\nc\n"))
          (of_string_exn {|
|
 a
 b
 c
|})
      )
  ; "yamlstring-3-literal-chomp" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a\nb\nc"))
          (of_string_exn {|
|-
 a
 b
 c
|})
      )
  ; "yamlstring-3-literal-add" >:: (fun ctxt ->
        assert_equal ~printer
          (`String ("a\nb\nc\n"))
          (of_string_exn {|
|+
 a
 b
 c
|})
      )
  ; "yamlstring-4" >:: (fun ctxt ->
        assert_equal ~printer
          (`A[`String ("a b c")])
          (of_string_exn {|-
  a
  b
  c
|})
      )
  ; "yamlstring-4-fold" >:: (fun ctxt ->
        assert_equal ~printer
          (`A ([`String ("a b c\n")]))
          (of_string_exn {|- >
 a
 b
 c
|})
      )
  ; "yamlstring-4-fold-chomp" >:: (fun ctxt ->
        assert_equal ~printer
          (`A ([`String ("a b c")]))
          (of_string_exn {|- >-
 a
 b
 c
|})
      )
  ; "yamlstring-5" >:: (fun ctxt ->
        assert_equal ~printer
          (`A[`String ("a b c")])
          (of_string_exn {|-
  a
  b
  c
|})
      )
  ; "yamlstring-5-fold" >:: (fun ctxt ->
        assert_equal ~printer
          (`A ([`String ("a b c\n")]))
          (of_string_exn {|- 
 >
 a
 b
 c
|})
      )
  ; "yamlstring-5-fold-2" >:: (fun ctxt ->
        assert_equal ~printer
          (`A ([`String ("a b c\n")]))
          (of_string_exn {|- 
 >a
  b
  c
|})
      )
  ; "yamlstring-6" >:: (fun ctxt ->
        assert_equal ~printer
          (`A[`String ("a b c")])
          (of_string_exn {|
-
 a
 b
 c
|})
      )
  ; "yamlstring-6-fold" >:: (fun ctxt ->
        assert_equal ~printer
          (`A[`String ("a b c\n")])
          (of_string_exn {|
- >
 a
 b
 c
|})
      )
  ; "yamlstring-6-fold-2" >:: (fun ctxt ->
       assert_raises_exn_pattern
         "[scalar_rawstring0_or_yamlstrings] expected after [must_fold_chomp_add] (in [block_members])"
          (fun () -> of_string_exn {|
- 
>
a
|})
      )
  ; "yamlstring-6-fold-3" >:: (fun ctxt ->
        assert_equal ~printer
          (`A[`String ("a b c\n")])
          (of_string_exn {|
-
> a
  b
  c
|})
      )



  ; "dots-1" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "EOI expected after [docs] (in [docs_eoi])"
          (fun () -> docs_of_string_exn {|
---
a: b
... foo
|})
      )
  ; "yamlstring-keys-1" >:: (fun ctxt ->
       assert_raises_exn_pattern
         "':' expected after [key_scalar] (in [block_members])"
          (fun () -> of_string_exn {|
a: c
b
d: e
|})
      )

  ]

let preview = "preview" >::: [
    "2.1" >:: (fun ctxt ->
        assert_equal ~printer
          (`A (
              [`String ("Mark McGwire")
              ; `String ("Sammy Sosa")
              ; `String ("Ken Griffey")]
            ))
        (of_string_exn {|- Mark McGwire
- Sammy Sosa
- Ken Griffey|})
      )
  ; "2.2" >:: (fun ctxt ->
        assert_equal ~printer
          (`O ([("hr", `Float (65.)); ("avg", `Float (0.278)); ("rbi", `Float (147.))]))
        (of_string_exn {|hr:  65    # Home runs
avg: 0.278 # Batting average
rbi: 147   # Runs Batted In|})
      )
  ; "2.3" >:: (fun ctxt ->
        assert_equal ~printer
          (`O (
              [("american",
                `A (
                  [`String ("Boston Red Sox"); `String ("Detroit Tigers");
                   `String ("New York Yankees")]
                ));
               ("national",
                `A (
                  [`String ("New York Mets"); `String ("Chicago Cubs");
                   `String ("Atlanta Braves")]
                ))
              ]
            ))
        (of_string_exn {|american:
  - Boston Red Sox
  - Detroit Tigers
  - New York Yankees
national:
  - New York Mets
  - Chicago Cubs
  - Atlanta Braves|})
      )
  ; "2.4" >:: (fun ctxt ->
        assert_equal ~printer
          (`A (
              [`O (
                  [("name", `String ("Mark McGwire")); ("hr", `Float (65.));
                   ("avg", `Float (0.278))]
                );
               `O (
                 [("name", `String ("Sammy Sosa")); ("hr", `Float (63.));
                  ("avg", `Float (0.288))]
               )
              ]
            ))
        (of_string_exn {|-
  name: Mark McGwire
  hr:   65
  avg:  0.278
-
  name: Sammy Sosa
  hr:   63
  avg:  0.288|})
      )
  ; "2.5" >:: (fun ctxt ->
        assert_equal ~printer
          (`A (
              [`A ([`String ("name"); `String ("hr"); `String ("avg")]);
               `A ([`String ("Mark McGwire"); `Float (65.); `Float (0.278)]);
               `A ([`String ("Sammy Sosa"); `Float (63.); `Float (0.288)])]
            ))
        (of_string_exn {|- [name        , hr, avg  ]
- [Mark McGwire, 65, 0.278]
- [Sammy Sosa  , 63, 0.288]

|})
      )
  ; "2.6" >:: (fun ctxt ->
        assert_equal ~printer
          (`O (
              [("Mark McGwire", `O ([("hr", `Float (65.)); ("avg", `Float (0.278))]));
               ("Sammy Sosa", `O ([("hr", `Float (63.)); ("avg", `Float (0.288))]))]
            ))
        (of_string_exn {|Mark McGwire: {hr: 65, avg: 0.278}
Sammy Sosa: {
    hr: 63,
    avg: 0.288
  }|})
      )
  ; "2.7" >:: (fun ctxt ->
        assert_equal ~printer:docs_printer
          [`A (
              [`String ("Mark McGwire"); `String ("Sammy Sosa"); `String ("Ken Griffey")
              ]
            );
           `A ([`String ("Chicago Cubs"); `String ("St Louis Cardinals")])]
        (docs_of_string_exn {|# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals|})
      )
  ; "2.8" >:: (fun ctxt ->
        assert_equal ~printer:docs_printer
          [`O (
              [("time", `String ("20:03:20")); ("player", `String ("Sammy Sosa"));
               ("action", `String ("strike (miss)"))]
            );
           `O (
             [("time", `String ("20:03:47")); ("player", `String ("Sammy Sosa"));
              ("action", `String ("grand slam"))]
           )
          ]
        (docs_of_string_exn {|---
time: "20:03:20"
player: Sammy Sosa
action: strike (miss)
...
---
time: "20:03:47"
player: Sammy Sosa
action: grand slam
...|})
      )
  ; "2.9" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("hr", `A ([`String ("Mark McGwire"); `String ("Sammy Sosa")]));
             ("rbi", `A ([`String ("Sammy Sosa"); `String ("Ken Griffey")]))]
          ))
        (of_string_exn {|---
hr: # 1998 hr ranking
  - Mark McGwire
  - Sammy Sosa
rbi:
  # 1998 rbi ranking
  - Sammy Sosa
  - Ken Griffey|})
      )
  ; "2.12" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`O ([("item", `String ("Super Hoop")); ("quantity", `Float (1.))]);
             `O ([("item", `String ("Basketball")); ("quantity", `Float (4.))]);
             `O ([("item", `String ("Big Shoes")); ("quantity", `Float (1.))])]
          ))
        (of_string_exn {|---
# Products purchased
- item    : Super Hoop
  quantity: 1
- item    : Basketball
  quantity: 4
- item    : Big Shoes
  quantity: 1
|})
      )
  ; "2.13" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("\\//||\\/||\n\
                   // ||  ||__"))
        (of_string_exn {|# ASCII Art
---
  R"(\//||\/||
     // ||  ||__)"|})
      )
  ; "2.14" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("Mark McGwire's year was crippled by a knee injury."))
        (of_string_exn {|--- >
  R"(Mark McGwire's
     year was crippled
     by a knee injury.)"|})
      )
  ; "2.15" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (
            "Sammy Sosa completed another fine season with great stats.\n\n\
            \  63 Home Runs\n\
            \  0.288 Batting Average\n\n\
             What a year!"
          ))
        (of_string_exn {|>
  R"(Sammy Sosa completed another
     fine season with great stats.

       63 Home Runs
       0.288 Batting Average

     What a year!)"|})
      )
  ; "2.16" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("name", `String ("Mark McGwire"));
             ("accomplishment",
              `String ("Mark set a major league home run record in 1998.\n"));
             ("stats", `String ("65 Home Runs\n0.278 Batting Average\n"))]
          ))
        (of_string_exn {|name: Mark McGwire
accomplishment: >
  Mark set a major league
  home run record in 1998.

stats: |
  65 Home Runs
  0.278 Batting Average

|})
      )
  ; "2.17" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("unicode", `String ("Sosa did fine.\226\152\186"));
             ("control", `String ("\b1998\t1999\t2000\n"));
             ("hex esc", `String ("\r\n is \r\n"));
             ("single", `String ("\"Howdy!\" he cried."));
             ("quoted", `String (" # Not a 'comment'."));
             ("tie-fighter", `String ("|\\-*-/|"))]
          ))
        (of_string_exn {|unicode: "Sosa did fine.\u263A"
control: "\b1998\t1999\t2000\n"
hex esc: "\x0d\x0a is \r\n"

single: '"Howdy!" he cried.'
quoted: ' # Not a ''comment''.'
'tie-fighter': '|\-*-/|'|})
      )
  ; "2.18" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("plain", `String ("This unquoted scalar spans many lines."));
             ("quoted", `String ("So does this quoted scalar.\n"))]
          ))
        (of_string_exn {|plain:
  This unquoted scalar
  spans many lines.

quoted: "So does this
  quoted scalar.\n"
|})
      )
  ; "2.19" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("canonical", `Float (12345.)); ("decimal", `Float (12345.));
             ("octal", `Float 12.); ("hexadecimal", `Float (12.))]
          ))
        (of_string_exn {|canonical: 12345
decimal: 12345
octal: 0o14
hexadecimal: 0xC
|})
      )
  ; "2.20" >:: (fun ctxt ->
      assert_equal ~printer
        ~cmp
        (`O (
            [("canonical", `Float (1230.15)); ("exponential", `Float (1230.15));
             ("fixed", `Float (1230.15));
             ("negative infinity", `Float (neg_infinity));
             ("not a number", `Float (nan))]
          ))
        (of_string_exn {|canonical: 1.23015e+3
exponential: 12.3015e+02
fixed: 1230.15
negative infinity: -.inf
not a number: .NaN|})
      )
  ; "2.21" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("null", `Null); ("booleans", `A ([`Bool (true); `Bool (false)]));
             ("string", `String ("012345"))]
          ))
        (of_string_exn {|null: null
booleans: [ true, false ]
string: '012345'|})
      )
  ; "2.22" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("canonical", `String ("2001-12-15T02:59:43.1Z"));
             ("iso8601", `String ("2001-12-14t21:59:43.10-05:00"));
             ("spaced", `String ("2001-12-14 21:59:43.10 -5"));
             ("date", `String ("2002-12-14"))]
          ))
        (of_string_exn {|canonical: "2001-12-15T02:59:43.1Z"
iso8601: "2001-12-14t21:59:43.10-05:00"
spaced: "2001-12-14 21:59:43.10 -5"
date: "2002-12-14"|})
      )
  ; "2.23" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("not-date", `String ("2002-04-28"));
             ("picture",
              `String (
                "R0lGODlhDAAMAIQAAP//9/X\n17unp5WZmZgAAAOfn515eXv\nPz7Y6OjuDg4J+fn5OTk6enp\n56enmleECcgggoBADs=\n"
              ));
             ("application specific tag",
              `String (
                "The semantics of the tag\nabove may be different for\ndifferent documents.\n"
              ))
            ]
          ))
        (of_string_exn {|---
"not-date": "2002-04-28"

picture: |+
 R"(R0lGODlhDAAMAIQAAP//9/X
    17unp5WZmZgAAAOfn515eXv
    Pz7Y6OjuDg4J+fn5OTk6enp
    56enmleECcgggoBADs=)"

application specific tag: |
    The semantics of the tag
    above may be different for
    different documents.

|})
      )
  ; "2.26" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`O ([("Mark McGwire", `Float (65.))]);
             `O ([("Sammy Sosa", `Float (63.))]); `O ([("Ken Griffy", `Float (58.))])]
          ))
        (of_string_exn {|# Ordered maps are represented as
# A sequence of mappings, with
# each mapping having one key
---
- Mark McGwire: 65
- Sammy Sosa: 63
- Ken Griffy: 58|})
      )
  ; "2.27" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("invoice", `Float (34843.)); ("date", `String ("2001-01-23"));
             ("bill-to",
              `O (
                [("given", `String ("Chris")); ("family", `String ("Dumars"));
                 ("address",
                  `O (
                    [("lines", `String ("458 Walkman Dr.\nSuite #292"));
                     ("city", `String ("Royal Oak")); ("state", `String ("MI"));
                     ("postal", `Float (48046.))]
                  ))
                ]
              ));
             ("ship-to", `String ("bill-to"));
             ("product",
              `A (
                [`O (
                    [("sku", `String ("BL394D")); ("quantity", `Float (4.));
                     ("description", `String ("Basketball")); ("price", `Float (450.))
                    ]
                  );
                 `O (
                   [("sku", `String ("BL4438H")); ("quantity", `Float (1.));
                    ("description", `String ("Super Hoop"));
                    ("price", `Float (2392.))]
                 )
                ]
              ));
             ("tax", `Float (251.42)); ("total", `Float (4443.52));
             ("comments",
              `String (
                "Late afternoon is best.\nBackup contact is Nancy\nBillsmer @ 338-4338."
              ))
            ]
          ))
        (of_string_exn {|---
invoice: 34843
date   : "2001-01-23"
"bill-to":
    given  : Chris
    family : Dumars
    address:
        lines: |
         R"(458 Walkman Dr.
            Suite #292)"
        city    : Royal Oak
        state   : MI
        postal  : 48046
"ship-to": "bill-to"
product:
    - sku         : BL394D
      quantity    : 4
      description : Basketball
      price       : 450.00
    - sku         : BL4438H
      quantity    : 1
      description : Super Hoop
      price       : 2392.00
tax  : 251.42
total: 4443.52
comments:
 R"(Late afternoon is best.
    Backup contact is Nancy
    Billsmer @ 338-4338.)"|})
      )
  ; "2.28" >:: (fun ctxt ->
      assert_equal ~printer:docs_printer
        [`O (
            [("Time", `String ("2001-11-23 15:01:42 -5")); ("User", `String ("ed"));
             ("Warning", `String ("This is an error message for the log file"))]
          );
         `O (
           [("Time", `String ("2001-11-23 15:02:31 -5")); ("User", `String ("ed"));
            ("Warning", `String ("A slightly different error message."))]
         );
         `O (
           [("Date", `String ("2001-11-23 15:03:17 -5")); ("User", `String ("ed"));
            ("Fatal", `String ("Unknown variable \"bar\""));
            ("Stack",
             `A (
               [`O (
                   [("file", `String ("TopClass.py")); ("line", `Float (23.));
                    ("code", `String ("x = MoreObject(\"345\\n\")"))]
                 );
                `O (
                  [("file", `String ("MoreClass.py")); ("line", `Float (58.));
                   ("code", `String ("foo = bar"))]
                )
               ]
             ))
           ]
         )
        ]
        (docs_of_string_exn {|---
Time: "2001-11-23 15:01:42 -5"
User: ed
Warning:
  This is an error message
  for the log file
---
Time: "2001-11-23 15:02:31 -5"
User: ed
Warning:
  A slightly different error
  message.
---
Date: "2001-11-23 15:03:17 -5"
User: ed
Fatal:
  R"(Unknown variable "bar")"
Stack:
  - file: TopClass.py
    line: 23
    code: |
      R"(x = MoreObject("345\n"))"
  - file: MoreClass.py
    line: 58
    code: |
      R"(foo = bar)"
|})
      )
  ]

let characters = "characters" >::: [
    "5.3" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("sequence", `A ([`String ("one"); `String ("two")]));
             ("mapping", `O ([("sky", `String ("blue")); ("sea", `String ("green"))]))
            ]
          ))
        (of_string_exn {|sequence:
  - one
  - two
mapping:
  sky:
    blue
  sea : green|})
      )
  ; "5.4" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("sequence", `A ([`String ("one"); `String ("two")]));
             ("mapping", `O ([("sky", `String ("blue")); ("sea", `String ("green"))]))
            ]
          ))
        (of_string_exn {|
sequence: [ one, two ]
mapping: { sky: blue, sea: green }|})
      )
  ; "5.7" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("literal", `String ("some\ntext\n")); ("folded", `String ("some text"))]))
        (of_string_exn {|literal: |
  some
  text

folded: >-
  some
  text|})
      )
  ; "5.8" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("single", `String ("text")); ("double", `String ("text"))]))
        (of_string_exn {|
single: 'text'
double: "text"|})
      )
  ; "5.9" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("text"))
        (of_string_exn {|
--- text|})
      )
  ; "5.10" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("commercial-at", `String ("@text")); ("grave-accent", `String ("`text"))]
          ))
        (of_string_exn {|"commercial-at": @text
"grave-accent": `text|})
      )
  ; "5.11" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("Line break (no glyph)\nLine break (glyphed)\n"))
        (of_string_exn {||
  Line break (no glyph)
  Line break (glyphed)

|})
      )
  ; "5.12" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("quoted", `String ("Quoted \t"));
             ("block", `String ("void main() {\n\
                                 \tprintf(\"Hello, world!\\n\");\n\
                                 }"))]
          ))
        (of_string_exn {|# Tabs and spaces
quoted: "Quoted 	"
block:	|
  R"(void main() {
     	printf("Hello, world!\n");
     })"|})
      )
  ; "5.13" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("Fun with \\ \" \007 \b \027 \012 \n \r \t \011 \000   \160 \133 \226\128\168 \226\128\169 A A A"))
        (of_string_exn {|"Fun with \\
\" \a \b \e \f
\n \r \t \v \0
\  \_ \N \L \P
\x41 \u0041 \U00000041"|})
      )
  ; "5.14" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "Unexpected character"
        (fun () -> of_string_exn {|Bad escapes:
  "\c
  \xq-"|})
      )

  ]

let basic_structures = "basic structures" >::: [
    "6.1" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("Not indented",
              `O (
                [("By one space", `String ("By four\n  spaces\n"));
                 ("Flow style",
                  `A (
                    [`String ("By two"); `String ("Also by two");
                     `String ("Still by two")]
                  ))
                ]
              ))
            ]
          ))
        (of_string_exn {|  # Leading comment line spaces are
   # neither content nor indentation.
    
Not indented:
 By one space: |+
    R"(By four
         spaces)"
 Flow style: [    # Leading spaces
   By two,        # in flow style
  Also by two,    # are neither
  	Still by two   # content nor
    ]             # indentation.|})
      )
  ; "6.2" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("a", `A ([`String ("b"); `A ([`String ("c"); `String ("d")])]))]))
        (of_string_exn "\n\
a:\n\
\  -\tb\n\
\  -  -\tc\n\
\     - d")
      )
  ; "6.3" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`O ([("foo", `String ("bar"))]); `A ([`String ("baz"); `String ("baz")])]))
        (of_string_exn "- foo:\t bar\n\
                        - - baz\n\
                       \  -\tbaz")
      )
  ; "6.4" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("plain", `String ("text lines")); ("quoted", `String ("text lines"));
             ("block", `String ("text\n \tlines"))]
          ))
        (of_string_exn {|
plain: text
       lines
quoted: "text
  	lines"
block: |
  R"(text
      	lines)"|})
      )
  ; "6.5" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("Folding", `String ("Empty line\nas a line feed"));
             ("Chomping", `String ("Clipped empty lines\n"))]
          ))
        (of_string_exn {|
Folding:
  "Empty line
   	
  as a line feed"
Chomping: >
  Clipped empty lines

 |})
      )
  ; "6.6" >:: (fun ctxt ->
      assert_equal ~printer
          (`String ("trimmed\n\n\nas space"))
        (of_string_exn {|>
  R"(trimmed



     as
     space)"|})
      )
  ; "6.7" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("foo \n\t bar\nbaz\n"))
        (of_string_exn {|>+
  R"(foo 

     	 bar

     baz)"
|})
      )
  ; "6.8" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (" foo\nbar\nbaz "))
        (of_string_exn {|"
  foo 
 
  	 bar

  baz
"|})
      )
  ; "6.9" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("key", `String ("value"))]))
        (of_string_exn {|key:    # Comment
  value|})
      )
  ; "6.11" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("key", `String ("value"))]))
        (of_string_exn {|key:    # Comment
        # lines
  value
|})
      )

  ; "6.28" >:: (fun ctxt ->
      assert_equal ~printer
        (`A ([`String ("12"); `Float (12.); `Float (12.)]))
        (of_string_exn {|# Assuming conventional resolution:
- "12"
- 12
- 12|})
      )

  ]
let flow_styles = "flow styles" >::: [
    "7.2" >:: (fun ctxt ->
      assert_equal ~printer
          (`O ([("foo", `Null); ("null", `String ("bar"))]))
        (of_string_exn {|{
  foo : null,
  null : bar
}|})
      )
  ; "7.4" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("implicit block key",
              `A ([`O ([("implicit flow key", `String ("value"))])]))]
          ))
        (of_string_exn {|"implicit block key" : [
 { "implicit flow key" : value }
 ]|})
      )
  ; "7.5" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("folded to a space,\nto a line feed, or \t \tnon-content"))
        (of_string_exn {|"folded 
to a space,	
 
to a line feed, or 	\
 \ 	non-content"|})
      )
  ; "7.6" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (" 1st non-empty\n2nd non-empty 3rd non-empty "))
        (of_string_exn {|" 1st non-empty

 2nd non-empty 
	3rd non-empty "|})
      )
  ; "7.7" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("here's to \"quotes\""))
        (of_string_exn {| 'here''s to "quotes"'|})
      )
  ; "7.8" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("implicit block key",
              `A ([`O ([("implicit flow key", `String ("value"))])]))]
          ))
        (of_string_exn {|'implicit block key' : [
  { 'implicit flow key' : value }
 ]|})
      )
  ; "7.9" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (" 1st non-empty\n2nd non-empty 3rd non-empty "))
        (of_string_exn {|
' 1st non-empty

 2nd non-empty 
	3rd non-empty '|})
      )
  ; "7.10" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`String ("::vector"); `String (": - ()"); `String ("Up, up, and away!");
             `Float (-123.); `String ("http://example.com/foo#bar");
             `A (
               [`String ("::vector"); `String (": - ()");
                `String ("Up, up and away!"); `Float (-123.);
                `String ("http://example.com/foo#bar")]
             )
            ]
          ))
        (of_string_exn {|# Outside flow collection:
- "::vector"
- ": - ()"
- "Up, up, and away!"
- -123
- "http://example.com/foo#bar"
# Inside flow collection:
- [ "::vector",
  ": - ()",
  "Up, up and away!",
  -123,
  "http://example.com/foo#bar" ]
|})
      )
  ; "7.11" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("implicit block key",
              `A ([`O ([("implicit flow key", `String ("value"))])]))]
          ))
        (of_string_exn {|
implicit block key : [
  { implicit flow key : value }
 ]|})
      )
  ; "7.12" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("1st non-empty\n2nd non-empty 3rd non-empty"))
        (of_string_exn {|
 "1st non-empty

   2nd non-empty 
   3rd non-empty"|})
      )
  ; "7.13" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`A ([`String ("one"); `String ("two")]);
             `A ([`String ("three"); `String ("four")])]
          ))
        (of_string_exn {|- [ one, two ]
- [three ,four]|})
      )
  ; "7.14" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`String ("double quoted"); `String ("single quoted");
             `String ("plain text"); `A ([`String ("nested")]);
             `O ([("single", `String ("pair"))])]
          ))
        (of_string_exn {|[
"double
 quoted", 'single
           quoted',
'plain
 text', [ nested ],
{ single: pair }
]|})
      )
  ; "7.15" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`O ([("one", `String ("two")); ("three", `String ("four"))]);
             `O ([("five", `String ("six")); ("seven", `String ("eight"))])]
          ))
        (of_string_exn {|
- { one : two , three: four }
- {five: six,seven : eight}|})
      )
  ; "7.17" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("unquoted", `String ("separate")); ("http://foo.com", `Null);
             ("omitted value", `Null); ("null", `String ("omitted key"))]
          ))
        (of_string_exn {|{
unquoted : "separate",
"http://foo.com" : null,
omitted value: null,
null: omitted key
}|})
      )
  ; "7.18" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("adjacent", `String ("value")); ("readable", `String ("value"));
             ("empty", `Null)]
          ))
        (of_string_exn {|{
"adjacent":value,
"readable": value,
"empty": null
}|})
      )
  ; "7.19" >:: (fun ctxt ->
      assert_equal ~printer
        (`A ([`O ([("foo", `String ("bar"))])]))
        (of_string_exn {|[
{ foo: bar }
]|})
      )
  ; "7.20" >:: (fun ctxt ->
      assert_equal ~printer
        ( `A ([`O ([("foo bar", `String ("baz"))])]))
        (of_string_exn {|[
{ 'foo
 bar' : baz }
]|})
      )
  ; "7.23" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`A ([`String ("a"); `String ("b")]); `O ([("a", `String ("b"))]);
             `String ("a"); `String ("b"); `String ("c")]
          ))
        (of_string_exn {|- [ a, b ]
- { a: b }
- "a"
- 'b'
- c|})
      )
  ]

let block_styles = "block styles" >::: [
    "8.1" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`String ("literal\n"); `String (" folded\n"); `String ("keep\n\n");
             `String (" strip")]
          ))
        (of_string_exn {|
- | # Empty header
 literal

- |+ # Indentation indicator
 R"( folded)"
- # Chomping indicator
 R"(keep

    )"
- # Both indicators
  R"( strip)"
|})
      )
  ; "8.2" >:: (fun ctxt ->
      assert_equal ~printer
        (`A [`String"detected\n"; `String"\n\n# detected\n"; `String" explicit\n"; `String"\t detected\n"])
        (of_string_exn {|- |
 detected

-
 R"(
    
    # detected
    )"
-
 R"( explicit
    )"
- >
 R"(	
    detected
    )"|})
      )
  ; "8.3" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "dedent did not move back to previous indent position"
        (fun () -> of_string_exn {|-
  
 text
-
  text
 text
-
 text|})
      )
  ; "8.4" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("strip", `String ("text")); ("clip", `String ("text\n"));
             ("keep", `String ("text\n"))]
          ))
        (of_string_exn {|strip: |-
  text
clip: |
  text

keep: |
  text

|})
      )
  ; "8.5" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("strip", `String ("# text")); ("clip", `String ("# text\n"));
             ("keep", `String ("# text\n"))]
          ))
        (of_string_exn {| # Strip
  # Comments:
strip:
  "# text"
  
 # Clip
  # comments:

clip:
  R"(# text
     )"
 
 # Keep
  # comments:

keep: |+
  R"(# text)"

 # Trail
  # comments.|})
      )
  ; "8.6" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("strip", `String ("")); ("clip", `String ("")); ("keep", `String ("\n"))]
          ))
        (of_string_exn {|
strip: ""

clip: ""

keep: "\n"

|})
      )
  ; "8.7" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("literal\n\ttext\n"))
        (of_string_exn {||+
 R"(literal
    	text)"

|})
      )
  ; "8.8" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("\n\nliteral\n \n\ntext\n"))
        (of_string_exn {|
|
 R"(
    
    literal
     

    text
    )"
 # Comment|})
      )
  ; "8.9" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("folded text\n"))
        (of_string_exn {|>
 folded
 text


|})
      )
  ; "8.10" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (
      "\n\
      folded line\n\
      next line\n\
      \  * bullet\n\
      \n\
      \  * list\n\
      \  * lines\n\
      \n\
      last line\n"
          ))
        (of_string_exn {|>

 R"(
    folded
    line

    next
    line
      * bullet

      * list
      * lines

    last
    line
    )"

# Comment|})
      )
  ; "8.11" >:: (fun ctxt ->
      assert_equal ~printer
          (`String (
      "\n\
      folded line\n\
      next line\n\
      \  * bullet\n\
      \n\
      \  * list\n\
      \  * lines\n\
      \n\
      last line\n"
  ))
        (of_string_exn {|>
 R"(
    folded
    line

    next
    line
      * bullet

      * list
      * lines

    last
    line
    )"
# Comment|})
      )
  ; "8.12" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (
      "\n\
      folded line\n\
      next line\n\
      \  * bullet\n\
      \n\
      \  * list\n\
      \  * line\n\
      \n\
      last line\n"
          ))
        (of_string_exn {|>
R"(
   folded
   line

   next
   line
     * bullet

     * list
     * line

   last
   line
   )"
# Comment|})
      )
  ; "8.13" >:: (fun ctxt ->
      assert_equal ~printer
        (`String (
      "\n\
      folded line\n\
      next line\n\
      \  * bullet\n\
      \n\
      \  * list\n\
      \  * line\n\
      \n\
       last line\n"
    ))
        (of_string_exn {|>
R"(
   folded
   line

   next
   line
     * bullet

     * list
     * line

   last
   line
   )"
# Comment|})
      )
  ; "8.14" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("block sequence",
              `A ([`String ("one"); `O ([("two", `String ("three"))])]))]
          ))
        (of_string_exn {|block sequence:
  - one
  - two : three
|})
      )
  ; "8.15-busted" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "[block_json] expected after INDENT (in [block_json])"
        (fun () -> of_string_exn {|
- # Empty
- a
|})
      )
  ; "8.15" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`Null; `String ("block node\n"); `A ([`String ("one"); `String ("two")]);
             `O ([("one", `String ("two"))])]
          ))
        (of_string_exn {|
- null # Empty
- |
 block node

- - one # Compact
  - two # sequence
- one: two # Compact mapping|})
      )
  ; "8.16" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("block mapping", `O ([("key", `String ("value"))]))]))
        (of_string_exn {|block mapping:
 key: value
|})
      )
  ; "8.17" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("explicit key", `Null);
             ("block key\n", `A ([`String ("one"); `String ("two")]))]
          ))
        (of_string_exn {|
explicit key: null # Empty value
R"(block key
   )":
  - one # Explicit compact
  - two # block value
|})
      )
  ; "8.20" >:: (fun ctxt ->
      assert_equal ~printer
        (`A (
            [`String ("flow in block"); `String ("Block scalar\n");
             `O ([("foo", `String ("bar"))])]
          ))
        (of_string_exn {|-
  "flow in block"
- >
 Block scalar

- # Block collection
  foo : bar
|})
      )
  ; "8.21" >:: (fun ctxt ->
      assert_equal ~printer
        (`O ([("literal", `String ("value\n")); ("folded", `String ("value"))]))
        (of_string_exn {|literal: |
  value

folded: >-
 value|})
      )
  ; "8.22" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("sequence", `A ([`String ("entry"); `A ([`String ("nested")])]));
             ("mapping", `O ([("foo", `String ("bar"))]))]
          ))
        (of_string_exn {|sequence: 
 - entry
 -
  - nested
mapping:
 foo: bar|})
      )
  ]
let yaml_character_stream = "YAML Character Stream" >::: [
    "9.2" >:: (fun ctxt ->
      assert_equal ~printer
        (`String ("Document"))
        (of_string_exn {|%BS4J-1.0
---
Document
... # Suffix|})
      )
  ; "9.3" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "EOI expected after [docs] (in [docs_eoi])"
        (fun () -> docs_of_string_exn {|Bare
document
...
# No document
null
...
|
R"(%!PS-Adobe-2.0
   )" # Not the first line
...|})
      )
  ; "9.4" >:: (fun ctxt ->
      assert_equal ~printer:docs_printer
        [`O ([("matches %", `Float (20.))]); `Null]
        (docs_of_string_exn {|---
{ 'matches
%' : 20 }
...
---
# Empty
null
...|})
      )
  ; "9.5" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "EOI expected after [docs] (in [docs_eoi])"
        (fun() -> docs_of_string_exn {|%BS4J-1.0
--- |
R"(%!PS-Adobe-2.0
   )"
...
%YAML 1.2
...
---
# Empty
null
...|})
      )
  ; "9.6" >:: (fun ctxt ->
      assert_raises_exn_pattern
        "EOI expected after [docs] (in [docs_eoi])"
        (fun () -> docs_of_string_exn {|Document
...
---
# Empty
null
...
%YAML 1.2
...
---
matches %: 20|})
      )
  ]

let recommended_schemas = "Recommended Schemas" >::: [
    "10.1" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("Block style",
              `O (
                [("Clark", `String ("Evans")); ("Ingy", `String ("d\195\182t Net"));
                 ("Oren", `String ("Ben-Kiki"))]
              ));
             ("Flow style",
              `O (
                [("Clark", `String ("Evans")); ("Ingy", `String ("d\195\182t Net"));
                 ("Oren", `String ("Ben-Kiki"))]
              ))
            ]
          ))
        (of_string_exn {|Block style:
  Clark : Evans
  Ingy  : döt Net
  Oren  : "Ben-Kiki"

Flow style: { Clark: Evans, Ingy: döt Net, Oren: "Ben-Kiki" }|})
      )
  ; "10.2" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("Block style",
              `A (
                [`String ("Clark Evans"); `String ("Ingy d\195\182t Net");
                 `String ("Oren Ben-Kiki")]
              ));
             ("Flow style",
              `A (
                [`String ("Clark Evans"); `String ("Ingy d\195\182t Net");
                 `String ("Oren Ben-Kiki")]
              ))
            ]
          ))
        (of_string_exn {|Block style: 
 - Clark Evans
 - Ingy döt Net
 - "Oren Ben-Kiki"

Flow style: [ Clark Evans, Ingy döt Net, "Oren Ben-Kiki" ]|})
      )
  ; "10.3" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("Block style", `String ("String: just a theory."));
             ("Flow style", `String ("String: just a theory."))]
          ))
        (of_string_exn {|Block style:
  "String: just a theory."

Flow style: "String: just a theory."|})
      )
  ; "10.4" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("null", `String ("value for null key")); ("key with null value", `Null)]))
        (of_string_exn {|null: value for null key
key with null value: null|})
      )
  ; "10.5" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("YAML is a superset of JSON", `Bool (true));
             ("Pluto is a planet", `Bool (false))]
          ))
        (of_string_exn {|
YAML is a superset of JSON: true
Pluto is a planet: false|})
      )
  ; "10.6" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("negative", `Float (-12.)); ("zero", `Float (0.));
             ("positive", `Float (34.))]
          ))
        (of_string_exn {|negative: -12
zero: 0
positive: 34|})
      )
  ; "10.7" >:: (fun ctxt ->
      assert_equal ~printer
        ~cmp
        (`O (
            [("negative", `Float (-1.)); ("zero", `Float (0.));
             ("positive", `Float (23000.)); ("infinity", `Float (infinity));
             ("not a number", `Float (nan))]
          ))
        (of_string_exn {|negative: -1
zero: 0
positive: 2.3e4
infinity: .inf
not a number: .NaN|})
      )
  ; "10.8" >:: (fun ctxt ->
      assert_equal ~printer
        (`O (
            [("A null", `Null); ("Booleans", `A ([`Bool (true); `Bool (false)]));
             ("Integers", `A ([`Float (0.); `Float (-0.); `Float (3.); `Float (-19.)]));
             ("Floats",
              `A ([`Float (0.); `Float (-0.); `Float (12000.); `Float (-200000.)]));
             ("Invalid",
              `A ([`String"True"; `String"Null"; `Float (7.); `Float (58.); `Float 12.3]))
            ]
          ))
        (of_string_exn {|
A null: null
Booleans: [ true, false ]
Integers: [ 0, -0, 3, -19 ]
Floats: [ 0., -0.0, 12e03, -2E+05 ]
Invalid: [ True, Null, 0o7, 0x3A, 12.3 ]|})
      )
  ; "10.9" >:: (fun ctxt ->
      assert_equal ~printer
        ~cmp
        (`O (
            [("A null", `Null); ("Also a null", `Null); ("Not a null", `String (""));
             ("Booleans",
              `A ([`Bool (true); `String"True"; `Bool (false); `String"FALSE"]));
             ("Integers",
              `A ([`Float (0.); `Float (7.); `Float (58.); `Float (-19.)]));
             ("Floats",
              `A (
                [`Float (0.); `Float (-0.); `Float (0.5); `Float (12000.);
                 `Float (-200000.)]
              ));
             ("Also floats",
              `A (
                [`Float (infinity); `String ("-.Inf"); `Float (nan)
                ]
              ))
            ]
          ))
        (of_string_exn {|
A null: null
Also a null: null # Empty
Not a null: ""
Booleans: [ true, True, false, FALSE ]
Integers: [ 0, 0o7, 0x3A, -19 ]
Floats: [ 0., -0.0, .5, 12e03, -2E+05 ]
Also floats: [ .inf, '-.Inf', .NaN ]|})
      )
  ]

let tests = "all" >::: [
    lexing
  ; parsing
  ; preview
  ; characters
  ; basic_structures
  ; flow_styles
  ; block_styles
  ; yaml_character_stream
  ; recommended_schemas
]

if not !Sys.interactive then
  run_test_tt_main tests
;;
