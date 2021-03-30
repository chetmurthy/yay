open Asttools ;
open Jsontoken ;
open Jsontypes ;
open Pa_ppx_base.Pp_MLast ;
open Pa_ppx_runtime.Exceptions ;

type t += [
    Exc of Ploc.t and t[@rebind_to Ploc.Exc;][@name "Ploc.Exc";]
] [@@deriving show;]
;

value print_exn exn = Some (show exn) ;
Printexc.register_printer print_exn ;

value positions_to_loc ?{comments=""} (spos, epos) =
  let open Lexing in
  Ploc.make_loc spos.pos_fname spos.pos_lnum spos.pos_bol (spos.pos_cnum, epos.pos_cnum) comments
;

value compatible_lexer lb =
  let ((tok, pos) as t) = Jsontoken.jsontoken lb in
  let pos = positions_to_loc pos in
  let tok = match tok with [
    BS4J s -> ("BS4J",s)
  | LBRACKET -> ("","[")
  | RBRACKET -> ("","]")
  | LBRACE -> ("","{")
  | RBRACE -> ("","}")
  | COLON -> ("",":")
  | COMMA -> ("",",")
  | DASH -> ("","-")
  | DASHDASHDASH -> ("","---")
  | DOTDOTDOT -> ("","...")
  | BAR -> ("","|")
  | BARDASH -> ("","|-")
  | BARPLUS -> ("","|+")
  | GT -> ("",">")
  | GTDASH -> ("",">-")
  | GTPLUS -> ("",">+")
  | YAMLSTRING "false" -> ("","false")
  | YAMLSTRING "true" -> ("","true")
  | YAMLSTRING "null" -> ("","null")
  | YAMLSTRING s -> ("YAMLSTRING",s)
  | YAMLSQSTRING s -> ("YAMLSQSTRING",s)
  | YAMLDQSTRING s -> ("YAMLDQSTRING",s)
  | RAWSTRING s ->
    ("RAWSTRING",s)
  | INDENT _ _ -> ("INDENT","")
  | DEDENT _ _ -> ("DEDENT","")
  | DECIMAL s -> ("DECIMAL",s)
  | HEXADECIMAL s -> ("HEXADECIMAL",s)
  | OCTAL s -> ("OCTAL",s)
  | JSONSTRING s -> ("JSONSTRING", s)
  | EOF -> ("EOI","")

  ] in
  (tok, pos)
;

value lex_string s =
  let st = St.mk (Sedlexing.Latin1.from_gen (gen_of_string s)) in
  let rec lexrec acc =
    match compatible_lexer st with [
      (("EOI",_),_) as t -> List.rev [t::acc]
    | t -> lexrec [t::acc]
    ] in lexrec []
;

(* camlp5r *)
(* calc.ml,v *)

value input_file = ref "" ;
value nonws_re = Pcre.regexp "\\S" ;
value has_nonws s = Pcre.pmatch ~{rex=nonws_re} s;

value lexer_func_of_sedlex_state_located lexfun cs =
  let read1 () =
    try Some (Stream.next cs) with [ Stream.Failure -> None ] in
  let lb = St.mk (Sedlexing.Latin1.from_gen read1)
  in
  let next_token_func () = lexfun lb in
  Plexing.make_stream_and_location next_token_func
;

value lexer = lexer_func_of_sedlex_state_located compatible_lexer ;
value lexer = {Plexing.tok_func = lexer;
 Plexing.tok_using _ = (); Plexing.tok_removing _ = ();
 Plexing.tok_match = Plexing.default_match;
 Plexing.tok_text = Plexing.lexer_text;
 Plexing.tok_comm = None} ;

value g = Grammar.gcreate lexer;
value (json : Grammar.Entry.e json) = Grammar.Entry.create g "json";
value (json_eoi : Grammar.Entry.e json) = Grammar.Entry.create g "json_eoi";
value (json_stream : Grammar.Entry.e (list json)) = Grammar.Entry.create g "json_stream";
value (json_stream_eoi : Grammar.Entry.e (list json)) = Grammar.Entry.create g "json_stream_eoi";
value (block_json : Grammar.Entry.e json) = Grammar.Entry.create g "block_json";
value block_json_eoi = Grammar.Entry.create g "block_json_eoi";
value (doc : Grammar.Entry.e json) = Grammar.Entry.create g "doc";
value (doc_eoi : Grammar.Entry.e json) = Grammar.Entry.create g "doc_eoi";
value (docs : Grammar.Entry.e (list json)) = Grammar.Entry.create g "docs";
value (docs_eoi : Grammar.Entry.e (list json)) = Grammar.Entry.create g "docs_eoi";
value (flow_json : Grammar.Entry.e json) = Grammar.Entry.create g "flow_json";
value (flow_json_eoi : Grammar.Entry.e json) = Grammar.Entry.create g "flow_json_eoi";
value (flow_json_stream : Grammar.Entry.e (list json)) = Grammar.Entry.create g "flow_json_stream";
value (flow_json_stream_eoi : Grammar.Entry.e (list json)) = Grammar.Entry.create g "flow_json_stream_eoi";

value string_of_scalar = fun [
  `String s -> s
| `Float f -> string_of_float f
| `Null -> "null"
| `Bool True -> "true"
| `Bool False -> "false"
| _ -> assert False
]
;

EXTEND
  GLOBAL:
    json json_eoi
    json_stream json_stream_eoi
    flow_json flow_json_eoi
    flow_json_stream flow_json_stream_eoi
    block_json block_json_eoi
    doc doc_eoi
    docs docs_eoi
    ;
  doc: [ [ v=block_json -> v
         | "---" ; v=block_json -> v
         | "---" ; v=block_json ; "..." -> v
         | v=block_json ; "..." -> v
    ] ]
  ;
  delim_doc: [ [ "---" ; v=block_json -> v
               | "---" ; v=block_json ; OPT "..." -> v

    ] ]
  ;
  docs: [ [ l = LIST1 delim_doc -> l
          | v=block_json -> [v]
    ] ]
  ;

  flow_json:
    [ [ s = flow_scalar -> s

      | "[" ; l = flow_json_comma_list ; "]" -> `List l
      | "{" ; l = flow_json_pair_comma_list ; "}" -> `Assoc l
    ] ]
  ;

  scalar_rawstring0_or_yamlstrings:
    [ [ (s,l) = [ s = RAWSTRING -> (s,loc) ] ->
        Left(s, l)

      | s = YAMLSTRING ; l = LIST0 [ s = YAMLSTRING -> s ] -> Right [s::l]

      | INDENT ; rv = scalar_rawstring0_or_yamlstrings ; DEDENT -> rv
    ] ]
  ;

  block_members:
    [ [ fca = must_fold_chomp_add ; r_or_y = scalar_rawstring0_or_yamlstrings ->
        match r_or_y with [
          Left (s,l) ->
          let s = unquote_rawstring fca l s in
          `String s
        | Right l ->
          `String (foldchomp_yamlstrings fca l)
        ]

      | s = scalar_nonstring -> s
      | s = scalar_nonstring ; ":" ; v=block_json ;
         l = LIST0 [ s=key_scalar ; ":" ; v=block_json -> (string_of_scalar s,v) ]
         -> `Assoc [(string_of_scalar s,v) :: l]

      | (s,l) = scalar_rawstring0 ->
         let s = unquote_rawstring (False, False, False) l s in
        `String s

      | (s,l) = scalar_rawstring0 ; ":" ; v=block_json ;
         rest = LIST0 [ s=key_scalar ; ":" ; v=block_json -> (string_of_scalar s,v) ] ->
         let s = unquote_rawstring (False, False, False) l s in
         `Assoc [(s,v) :: rest]

      | s = YAMLSTRING ; l = LIST0 [ s = YAMLSTRING -> s ] -> `String (foldchomp_yamlstrings (True, True, False) [s::l])

      | s = YAMLSTRING ; ":" ; v=block_json ;
         l = LIST0 [ s=key_scalar ; ":" ; v=block_json -> (string_of_scalar s,v) ]
         -> `Assoc [(s,v) :: l]

      | s = scalar_other_string -> `String s
      | s = scalar_other_string ; ":" ; v=block_json ;
         l = LIST0 [ s=key_scalar ; ":" ; v=block_json -> (string_of_scalar s,v) ]
         -> `Assoc [(s,v) :: l]


      | "-" ; v=block_json ;
         l = LIST0 [ "-" ; v=block_json -> v ]
         -> `List [v :: l]
      ] ]
    ;

  flow_json_comma_list:
    [ [
      v = flow_json -> [v]
    | v = flow_json ; "," -> [v]
    | v = flow_json ; "," ; vl = flow_json_comma_list -> [v::vl]
    | -> []
    ] ]
    ;

  flow_json_pair_comma_list:
    [ [
      s = flow_scalar ; ":" ; v=flow_json -> [(string_of_scalar s,v)]
    | s = flow_scalar ; ":" ; v=flow_json ; "," -> [(string_of_scalar s,v)]
    | s = flow_scalar ; ":" ; v=flow_json ; "," ; vl = flow_json_pair_comma_list -> [(string_of_scalar s,v)::vl]
    | -> []
    ] ]
    ;

  block_json:
    [ [ s = block_members -> s

      | INDENT ; v=block_json ; DEDENT -> v

      | "[" ; l = flow_json_comma_list ; "]" -> `List l
      | "{" ; l = flow_json_pair_comma_list ; "}" -> `Assoc l
    ] ]
  ;

  must_fold_chomp_add :
    [ [ ">" -> (True, False, False)
      | "|" -> (False, False, False)
      | ">-" -> (True, True, False)
      | "|-" -> (False, True, False)
      | ">+" -> (True, False, True)
      | "|+" -> (False, False, True)
      ] ]
    ;

  fold_chomp_add :
    [ [ fc = must_fold_chomp_add -> fc
      | -> (False, False, False)
      ] ]
    ;
  
  scalar_rawstring:
    [ [ fc = fold_chomp_add ; (s,l) = scalar_rawstring0 -> (fc, s, l)
    ] ]
  ;
  
  scalar_rawstring0:
    [ [ (s,l) = [ s = RAWSTRING -> (s,loc) ] ->
        (s, l)

      | (s,l) = [ INDENT ; (s,l) = scalar_rawstring0 ; DEDENT -> (s,l) ] ->
        (s, l)
    ] ]
  ;

  scalar_other_string:
    [ [ s = JSONSTRING -> (unquote_jsonstring s)
      | s=YAMLSQSTRING -> (unquote_yaml_sqstring s)
      | s=YAMLDQSTRING -> (unquote_yaml_dqstring s)
    ] ]
  ;

  scalar_nonstring:
    [ [ n = DECIMAL -> `Float (if n = ".NaN" then nan
                               else if n = ".inf" then infinity
                               else if n = "-.inf" then neg_infinity
                               else convert_float n)
      | n = HEXADECIMAL -> `Float (float_of_int (int_of_string n))
      | n = OCTAL -> `Float (float_of_int (int_of_string n))
      | "null" -> `Null
      | "true" -> `Bool True
      | "false" -> `Bool False
    ] ]
  ;

  key_scalar:
    [ [ (s, l) = scalar_rawstring0 -> `String (unquote_rawstring (False, False, False) l s)
      | s = YAMLSTRING -> `String s
      | s = scalar_other_string -> `String s
      | s = scalar_nonstring -> s
    ] ]
  ;

  flow_scalar:
    [ [ fca = fold_chomp_add ; (s,l) = [ s = RAWSTRING -> (s,loc) ] ->
        `String (unquote_rawstring fca l s)

      | s = YAMLSTRING -> `String s
      | s = JSONSTRING -> `String (unquote_jsonstring s)
      | s=YAMLSQSTRING -> `String (unquote_yaml_sqstring s)
      | s=YAMLDQSTRING -> `String (unquote_yaml_dqstring s)
      | n = DECIMAL -> `Float (if n = ".NaN" then nan
                               else if n = ".inf" then infinity
                               else if n = "-.inf" then neg_infinity
                               else convert_float n)
      | n = HEXADECIMAL -> `Float (float_of_int (int_of_string n))
      | n = OCTAL -> `Float (float_of_int (int_of_string n))
      | "null" -> `Null
      | "true" -> `Bool True
      | "false" -> `Bool False
    ] ]
  ;

  flow_json_stream:
    [ [ l = LIST0 flow_json -> l ] ]
    ;

  json:
    [ [ s = scalar -> s

      | "[" ; l = LIST0 json SEP "," ; "]" -> `List l
      | "{" ; l = LIST0 [ s = json_string ; ":" ; v=json -> (s,v) ] SEP "," ; "}" -> `Assoc l
    ] ]
  ;

  json_string:
    [ [ s = YAMLDQSTRING -> unquote_jsonstring ~{prefixed=False} s
    ] ]
  ;

  scalar:
    [ [ s = YAMLDQSTRING -> `String (unquote_jsonstring ~{prefixed=False} s)
      | n = DECIMAL -> `Float (if n = ".NaN" then nan
                               else if n = ".inf" then infinity
                               else if n = "-.inf" then neg_infinity
                               else convert_float ~{json=True} n)
      | n = HEXADECIMAL -> Ploc.raise loc (Failure Fmt.(str "BS4J(JSON mode): hexadecimals are not permitted %a" Dump.string n))
      | n = OCTAL -> Ploc.raise loc (Failure Fmt.(str "BS4J(JSON mode): octals are not permitted %a" Dump.string n))
      | "null" -> `Null
      | "true" -> `Bool True
      | "false" -> `Bool False
    ] ]
  ;

  json_stream:
    [ [ l = LIST0 json -> l ] ]
    ;

  block_json_eoi : [ [ l = block_json ; EOI -> l ] ] ;
  json_eoi : [ [ l = json ; EOI -> l ] ] ;
  json_stream_eoi : [ [ l = json_stream ; EOI -> l ] ] ;
  flow_json_eoi : [ [ l = flow_json ; EOI -> l ] ] ;
  flow_json_stream_eoi : [ [ l = flow_json_stream ; EOI -> l ] ] ;
  doc_eoi : [ [ v = OPT BS4J ; l = doc ; EOI -> l ] ] ;
  docs_eoi : [ [ v = OPT BS4J ; l = docs ; EOI -> l ] ] ;
END;

value parse_block_json = Grammar.Entry.parse block_json ;
value parse_block_json_eoi = Grammar.Entry.parse block_json_eoi ;
value parse_json = Grammar.Entry.parse json ;
value parse_json_eoi = Grammar.Entry.parse json_eoi ;
value parse_json_stream = Grammar.Entry.parse json_stream ;
value parse_json_stream_eoi = Grammar.Entry.parse json_stream_eoi ;
value parse_flow_json = Grammar.Entry.parse flow_json ;
value parse_flow_json_eoi = Grammar.Entry.parse flow_json_eoi ;
value parse_flow_json_stream = Grammar.Entry.parse flow_json_stream ;
value parse_flow_json_stream_eoi = Grammar.Entry.parse flow_json_stream_eoi ;
value parse_doc = Grammar.Entry.parse doc ;
value parse_doc_eoi = Grammar.Entry.parse doc_eoi ;
value parse_docs = Grammar.Entry.parse docs ;
value parse_docs_eoi = Grammar.Entry.parse docs_eoi ;

value parse_string pf s =
  pf (Stream.of_string s)
;

value parse_channel pf ic =
  pf (Stream.of_channel ic)
;

value parse_file pf fname =
  let ic = open_in fname in
  let rv = pf (Stream.of_channel ic) in 
  do { close_in ic ; rv }
;
