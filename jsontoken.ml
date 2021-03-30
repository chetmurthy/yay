(*

   JSON-text = ws value ws

   These are the six structural characters:

      begin-array     = ws %x5B ws  ; [ left square bracket

      begin-object    = ws %x7B ws  ; { left curly bracket

      end-array       = ws %x5D ws  ; ] right square bracket

      end-object      = ws %x7D ws  ; } right curly bracket

      name-separator  = ws %x3A ws  ; : colon

      value-separator = ws %x2C ws  ; , comma

   Insignificant whitespace is allowed before or after any of the six
   structural characters.

      ws = *(
              %x20 /              ; Space
              %x09 /              ; Horizontal tab
              %x0A /              ; Line feed or New line
              %x0D )              ; Carriage return

      false null true

   The literal names MUST be lowercase.  No other literal names are
   allowed.

      value = false / null / true / object / array / number / string

      false = %x66.61.6c.73.65   ; false

      null  = %x6e.75.6c.6c      ; null

      true  = %x74.72.75.65      ; true


      object = begin-object [ member *( value-separator member ) ]
               end-object

      member = string name-separator value

   array = begin-array [ value *( value-separator value ) ] end-array


      number = [ minus ] int [ frac ] [ exp ]

      decimal-point = %x2E       ; .

      digit1-9 = %x31-39         ; 1-9

      e = %x65 / %x45            ; e E

      exp = e [ minus / plus ] 1*DIGIT

      frac = decimal-point 1*DIGIT

      int = zero / ( digit1-9 *DIGIT )

      minus = %x2D               ; -

      plus = %x2B                ; +

      zero = %x30                ; 0

      string = quotation-mark *char quotation-mark

      char = unescaped /
          escape (
              %x22 /          ; "    quotation mark  U+0022
              %x5C /          ; \    reverse solidus U+005C
              %x2F /          ; /    solidus         U+002F
              %x62 /          ; b    backspace       U+0008
              %x66 /          ; f    form feed       U+000C
              %x6E /          ; n    line feed       U+000A
              %x72 /          ; r    carriage return U+000D
              %x74 /          ; t    tab             U+0009
              %x75 4HEXDIG )  ; uXXXX                U+XXXX

      escape = %x5C              ; \

      quotation-mark = %x22      ; "

      unescaped = %x20-21 / %x23-5B / %x5D-10FFFF


*)

let gen_of_string s =
  let pos = ref 0 in
  fun () ->
    if !pos = String.length s then None
    else let c = String.get s !pos in
      pos := !pos + 1 ;
      Some c

let linews = [%sedlex.regexp? ' ' | '\t' | '\r']

let octdigit = [%sedlex.regexp? '0'..'7']
let digit = [%sedlex.regexp? '0'..'9']
let hexdigit = [%sedlex.regexp? '0'..'9' | 'a'..'f' | 'A'..'F']
let int = [%sedlex.regexp? '0' | ( ('1'..'9') , (Star digit) )]
let frac = [%sedlex.regexp? '.' , (Star digit)]
let ne_frac = [%sedlex.regexp? '.' , (Plus digit)]
let exp = [%sedlex.regexp? ('e' | 'E') , (Opt ('-' | '+')) , (Plus digit)]
let decimal_float_number = [%sedlex.regexp? (Opt '-') , ((int , (Opt frac) , (Opt exp)) | (ne_frac, Opt exp))]
let json_number = [%sedlex.regexp? (Opt '-') , int, Opt ne_frac, Opt exp]
let decimal_float_not_numbers = [%sedlex.regexp? ".inf" | "-.inf" | ".NaN"]
let decimal_float = [%sedlex.regexp? decimal_float_number | decimal_float_not_numbers]
let hexadecimal_integer = [%sedlex.regexp?  (Opt '-') , "0x" , Plus(hexdigit)]
let octal_integer = [%sedlex.regexp?  (Opt '-') , "0o" , Plus(octdigit)]

let letter = [%sedlex.regexp? 'a'..'z'|'A'..'Z']

let alphanum = [%sedlex.regexp? (letter|digit)]
let ident = [%sedlex.regexp? letter, Star alphanum]

let json_unescaped = [%sedlex.regexp? 0x20 .. 0x21 | 0x23 .. 0x5B | 0x5D .. 0x10FFFF ]
let json_escaped = [%sedlex.regexp? "\\" , ( 0x22 | 0x5C | 0x2F | 0x62 | 0x66 | 0x6E | 0x72 | 0x74 | (0x75, Rep(hexdigit,4)) ) ]
let json_string_char = [%sedlex.regexp? (json_unescaped | json_escaped ) ]
let json_string = [%sedlex.regexp?  "J\"" , (Star json_string_char) , '"']

let yamlscalar_char = [%sedlex.regexp? Compl (Chars "-[]{}|>:,#/\\\"\r\n'") ]
let yamlscalar_startchar = [%sedlex.regexp? Sub (yamlscalar_char, (linews| '.' | '!' | '&' | '*')) ]
let yamlscalar_endchar = [%sedlex.regexp? Sub (yamlscalar_char, linews) ]
let yamlscalar = [%sedlex.regexp?  yamlscalar_startchar, Opt (Star yamlscalar_char, yamlscalar_endchar) ]

let yaml_basic_string_char = [%sedlex.regexp? 0x9 | 0x20 .. 0x10ffff ]
let yaml_unescaped_sqstring_char = [%sedlex.regexp? Sub((yaml_basic_string_char | '\n'), '\'')  ]
let yaml_sqstring = [%sedlex.regexp?  "'" , (Star (yaml_unescaped_sqstring_char | "''")) , "'" ]

let yaml_basic_dqstring_char = [%sedlex.regexp? Sub(yaml_basic_string_char, ('"' | '\\')) ]
let yaml_dqstring_escaped_char = [%sedlex.regexp? "\\",
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
                                         | ( "x" , Rep(hexdigit,2)) (* ns-esc-8-bit *)
                                         | ( "u" , Rep(hexdigit,4)) (* ns-esc-16-bit *)
                                         | ( "U" , Rep(hexdigit,8)) (* ns-esc-32-bit *) ) ]
let yaml_dqstring_linebreak_1 = [%sedlex.regexp? ("\\", "\n", Star(' '|'\t'), Opt("\\")) ]
let yaml_dqstring_linebreak_2 = [%sedlex.regexp? ("\n" , Star(' '|'\t')) ]
let yaml_dqstring_char = [%sedlex.regexp? (yaml_basic_dqstring_char | yaml_dqstring_escaped_char ) ]
let yaml_dqstring = [%sedlex.regexp? "\"" , (Star (yaml_dqstring_char | yaml_dqstring_linebreak_1 | Plus(yaml_dqstring_linebreak_2))), '"' ]

let perl_comment = [%sedlex.regexp? '#' , Star(Compl '\n') ]
let cpp_comment = [%sedlex.regexp? "//" , Star(Compl '\n') ]
let c_comment = [%sedlex.regexp? "/*" , Star(Compl '*'| "*", Compl '/'), Star '*', "*/" ]

let comment = [%sedlex.regexp? perl_comment | cpp_comment | c_comment ]

let convert_float ?(json=false) s =
  let lb = Sedlexing.Latin1.from_gen (gen_of_string s) in
  match%sedlex lb with
    json_number, eof ->
    float_of_string (Sedlexing.Latin1.lexeme lb)
  | _ ->
    if json then
      failwith "convert_float: not a JSON float"
    else
      float_of_string s

let foldchomp_yamlstrings (fold, chomp, add) l =
  assert (not (chomp && add)) ;
  let s = if fold then
      String.concat " " l
    else String.concat "\n" l in
  if chomp then s else s^"\n"

let is_high_surrogate i =
  0xD800 <= i && i <= 0xDBFF

let is_low_surrogate i =
  0xDC00 <= i && i <= 0xDFFF

let code_of_surrogate_pair i j =
  let high10 = i - 0xD800 in
  let low10 = j - 0xDC00 in
  0x10000 + ((high10 lsl 10) lor low10)

let unquote_jsonstring ?(prefixed=true) s =
  let buf = Buffer.create (String.length s) in
  let lb = Sedlexing.Latin1.from_gen (gen_of_string s) in
  let rec unrec0 () =
    if prefixed then
      match%sedlex lb with
        "J\"" -> unrec1 ()
      | _ -> failwith "unquote_jsonstring: unexpected character"
    else
      match%sedlex lb with
        "\"" -> unrec1 ()
      | _ -> failwith "unquote_jsonstring(not prefixed): unexpected character"

  and unrec1 () =
    match%sedlex lb with
      Plus json_unescaped ->
      Buffer.add_string buf (Sedlexing.Latin1.lexeme lb) ;
      unrec1 ()
    | "\\", '"' -> Buffer.add_char buf '"' ; unrec1 ()
    | "\\", '\\' -> Buffer.add_char buf '\\' ; unrec1 ()
    | "\\", '/' -> Buffer.add_char buf '/' ; unrec1 ()
    | "\\", 'b' -> Buffer.add_char buf '\b' ; unrec1 ()
    | "\\", 'f' -> Buffer.add_char buf '\x0c' ; unrec1 ()
    | "\\", 'n' -> Buffer.add_char buf '\n' ; unrec1 ()
    | "\\", 'r' -> Buffer.add_char buf '\r' ; unrec1 ()
    | "\\", 't' -> Buffer.add_char buf '\t' ; unrec1 ()
    | "\\", 'u', Rep(hexdigit,4) ->
      let s = Sedlexing.Latin1.sub_lexeme lb 2 4 in
      let n = int_of_string ("0x"^s) in
      if Uchar.is_valid n then begin
        Buffer.add_utf_8_uchar buf (Uchar.of_int n) ;
        unrec1 ()
      end
      else if is_high_surrogate n then
          unrec2 n
      else begin
        Buffer.add_utf_8_uchar buf (Uchar.unsafe_of_int n) ;
        unrec1 ()
      end

    | '"' ->
      Buffer.contents buf

    | _ -> failwith "unquote_jsonstring: internal error"

and unrec2 hi =
  match%sedlex lb with
  | "\\", 'u', Rep(hexdigit,4) ->
    let s = Sedlexing.Latin1.sub_lexeme lb 2 4 in
    let lo = int_of_string ("0x"^s) in
    if is_low_surrogate lo then
      let u = code_of_surrogate_pair hi lo in
      Buffer.add_utf_8_uchar buf (Uchar.of_int u) ;
      unrec1 ()
    else failwith Fmt.(str "unquote_jsonstring: invalid unicode surrogates: (0x%04x, 0x%04x)" hi lo)

  | _ ->
    failwith Fmt.(str "unquote_jsonstring: missing low surrogate after hi: 0x%04x" hi)

  in unrec0 ()
(*
let lex_re1 lb =
  match%sedlex lb with
    Plus yaml_dqstring_linebreak_1 -> Sedlexing.Latin1.lexeme lb
  | _ -> failwith "lex_re: failed"

let lex_re2 lb =
  match%sedlex lb with
    Plus yaml_dqstring_char -> Sedlexing.Latin1.lexeme lb
  | _ -> failwith "lex_re: failed"
*)
let unquote_yaml_sqstring s =
  let buf = Buffer.create (String.length s) in
  let lb = Sedlexing.Latin1.from_gen (gen_of_string s) in
  let rec unrec0 () =
    match%sedlex lb with
    | "'" -> unrec1 ()
    | _ -> failwith "unquote_yaml_sqstring: unexpected character"
  and unrec1 () =
    match%sedlex lb with
    | Sub(yaml_unescaped_sqstring_char, (linews|'\n')), Opt(Star (Sub(yaml_unescaped_sqstring_char,'\n')), Sub(yaml_unescaped_sqstring_char, (linews|'\n'))) -> 
      Buffer.add_string buf (Sedlexing.Latin1.lexeme lb) ;
      unrec1 ()

    | Star linews, "\n", Star linews ->
      unrec2 1

    | Plus linews ->
      Buffer.add_string buf (Sedlexing.Latin1.lexeme lb) ;
      unrec1 ()

    | "''" ->
      Buffer.add_char buf '\'' ;
      unrec1 ()
    | "'" -> Buffer.contents buf
    | _ -> failwith "unquote_yaml_sqstring: internal error"

  and unrec2 n =
    match%sedlex lb with
    | Star linews, "\n", Star linews ->
      unrec2 (n+1)

    | _ ->
      if n = 1 then begin
        Buffer.add_char buf ' ' ;
        unrec1 ()
      end
      else begin
        for i = 2 to n do
          Buffer.add_char buf '\n'
        done ;
        unrec1 ()
      end

  in unrec0 ()

let unquote_yaml_dqstring s =
  let buf = Buffer.create (String.length s) in
  let lb = Sedlexing.Latin1.from_gen (gen_of_string s) in
  let rec unrec0 () =
    match%sedlex lb with
    | "\"" -> unrec1 ()
    | _ -> failwith "unquote_yaml_dqstring: unexpected character"
  and unrec1 () =
    match%sedlex lb with
    | Star yaml_basic_dqstring_char, Sub(yaml_basic_dqstring_char, (' ' | '\t')) ->
      Buffer.add_string buf (Sedlexing.Latin1.lexeme lb) ;
      unrec1 ()
        
    | "\\", "0" (* ns-esc-null *) -> Buffer.add_char buf '\x00' ; unrec1 ()
    | "\\", "a" (* ns-esc-bell *) -> Buffer.add_char buf '\x07' ; unrec1 ()
    | "\\", "b" (* ns-esc-backspace *) -> Buffer.add_char buf '\b' ; unrec1 ()
    |  "\\", ("t" | "\t") (* ns-esc-horizontal-tab *) -> Buffer.add_char buf '\t' ; unrec1 ()
    | "\\", "n" (* ns-esc-line-feed *) -> Buffer.add_char buf '\n' ; unrec1 ()
    | "\\", "v" (* ns-esc-vertical-tab *) -> Buffer.add_char buf '\x0b' ; unrec1 ()
    | "\\", "f" (* ns-esc-form-feed *) -> Buffer.add_char buf '\x0c' ; unrec1 ()
    | "\\", "r" (* ns-esc-carriage-return *) -> Buffer.add_char buf '\r' ; unrec1 ()
    | "\\", "e" (* ns-esc-escape *) -> Buffer.add_char buf '\x1b' ; unrec1 ()
    | "\\", ' ' (* ns-esc-space *) -> Buffer.add_char buf ' ' ; unrec1 ()
    | "\\", '\"' (* ns-esc-double-quote *) -> Buffer.add_char buf '"' ; unrec1 ()
    | "\\", '/' (* ns-esc-slash *) -> Buffer.add_char buf '/' ; unrec1 ()
    | "\\", '\\' (* ns-esc-backslash *) -> Buffer.add_char buf '\\' ; unrec1 ()
    | "\\", 'N' (* ns-esc-next-line *) -> Buffer.add_char buf '\x85' ; unrec1 ()
    | "\\", '_' (*ns-esc-non-breaking-space *) -> Buffer.add_char buf '\xa0' ; unrec1 ()
    | "\\", "L" (* ns-esc-line-separator *) -> Buffer.add_utf_8_uchar buf (Uchar.of_int 0x2028) ; unrec1 ()
    | "\\", "P" (* ns-esc-paragraph-separator *) -> Buffer.add_utf_8_uchar buf (Uchar.of_int 0x2029) ; unrec1 ()
    | "\\", "x" , Rep(hexdigit,2) (* ns-esc-8-bit *) ->
      let n = int_of_string ("0x"^(String.sub (Sedlexing.Latin1.lexeme lb) 2 2)) in
      Buffer.add_utf_8_uchar buf (Uchar.of_int n) ; unrec1 ()
    | "\\", "u" , Rep(hexdigit,4) (* ns-esc-16-bit *) ->
      let n = int_of_string ("0x"^(String.sub (Sedlexing.Latin1.lexeme lb) 2 4)) in
      Buffer.add_utf_8_uchar buf (Uchar.of_int n) ; unrec1 ()

    | "\\", "U" , Rep(hexdigit,8) (* ns-esc-32-bit *) ->
      let n = int_of_string ("0x"^(String.sub (Sedlexing.Latin1.lexeme lb) 2 8)) in
      Buffer.add_utf_8_uchar buf (Uchar.of_int n) ; unrec1 ()

    | yaml_dqstring_linebreak_1 ->
      unrec1 ()

    | Star(' ' | '\t'), yaml_dqstring_linebreak_2 ->
      unrec2 1

    | Plus(' '|'\t') ->
      Buffer.add_string buf (Sedlexing.Latin1.lexeme lb) ;
      unrec1 ()

    | '"' -> Buffer.contents buf
    | _ -> failwith "unquote_yaml_dqstring: unexpected character"

  and unrec2 nbreaks =
    match%sedlex lb with
    | yaml_dqstring_linebreak_2 ->
      unrec2 (nbreaks+1)
    | _ ->
      if nbreaks = 1 then
        Buffer.add_char buf ' '
      else 
        for i = 2 to nbreaks do
          Buffer.add_char buf '\n'
        done ;
      unrec1 ()

  in unrec0 ()

let indented n s =
  let slen = String.length s in
  if slen < n then false
  else
    let rec irec ofs =
      if ofs = n then true
      else if String.get s ofs = ' ' then
        irec (ofs+1)
      else false
    in irec 0

let consume_indent n s =
  let slen = String.length s in
  if slen = 0 then ""
  else if indented n s then
    String.sub s n (slen - n)
  else failwith "consume_indent"

type linetype_t = MT | SP | TXT
let classify = function
    "" -> MT
  | s when String.get s 0 = ' ' -> SP
  | _ -> TXT

let group l =
  let rec grec acc (cls,sofaracc) = function
      [] -> List.rev ((cls,List.rev sofaracc)::acc)
    | h::t ->
      if classify h = cls then grec acc (cls,h::sofaracc) t
      else grec ((cls,List.rev sofaracc)::acc) (classify h,[h]) t
  in match l with
    [] -> []
  | h::t -> grec [] (classify h, [h]) t

let newlines l =
  String.concat "\n" (l@[""])

let rec frec acc = function
      [] -> List.rev acc

    | (TXT,l)::((MT,_)::[] as tl) ->
      frec ((String.concat " " l)::acc) tl

    | (TXT,l)::((MT,_)::(SP,_)::_ as tl) ->
      frec ("\n"::(String.concat " " l)::acc) tl

    | (TXT,l)::((SP,_)::_ as tl) ->
      frec ("\n"::(String.concat " " l)::acc) tl

    | (TXT,l)::(_::_ as tl) ->
      frec ((String.concat " " l)::acc) tl

    | (TXT,l)::[] ->
      frec ((String.concat " " l)::acc) []

    | (MT,l)::tl ->
      frec ((newlines l)::acc) tl

    | (SP,l)::tl ->
      frec ((newlines l)::acc) tl

let fold_groups l =
String.concat "" (frec [] l)

let fold_lines l =
  let l = group l in
  fold_groups l

let fold_lines0 l =
  let rec frec = function

    | l1::l2::tl when l1 <> "" && l2 <> "" && String.get l1 0 <> ' ' && String.get l2 0 <> ' ' ->
      l1::" "::(frec (l2::tl))

    | [l1] -> [l1]

    | l1::tl -> l1::"\n"::(frec tl)

    | [] -> []

  in String.concat "" (frec l)

let compute_rawstring_indent loc s =
  let indent = Ploc.first_pos loc - Ploc.bol_pos loc in
  let sofs = (String.index s '(') + 1 in
  indent + sofs

let unquote_rawstring (fold, chomp, add) loc s =
  assert (not (chomp && add)) ;
  let indent = compute_rawstring_indent loc s in
  let sofs = (String.index s '(') + 1 in
  let eofs = (String.rindex s ')') in
  if sofs = eofs then "" else
  let s = String.sub s sofs (eofs-sofs) in
  let l = String.split_on_char '\n' s in
  let l = (List.hd l) :: (List.map (consume_indent indent) (List.tl l)) in
  let l = if chomp then
      List.fold_right (fun s acc ->
          match (s,acc) with
            ("",[]) -> []
          | (h,t) -> h::t)
        l []
    else l in

  let s = if fold then
      fold_lines l
    else String.concat "\n" l in
  assert (String.length s > 0) ;
  if add then
    if String.get s (String.length s - 1) = '\n' then s else s^"\n"
  else s

type token =
  | BS4J of string
  | LBRACKET
  | RBRACKET
  | LBRACE
  | RBRACE
  | COLON
  | COMMA
  | DASH
  | DASHDASHDASH
  | DOTDOTDOT
  | BAR | BARDASH | BARPLUS
  | GT | GTDASH | GTPLUS
  | DECIMAL of string
  | HEXADECIMAL of string
  | OCTAL of string
  | JSONSTRING of string
  | RAWSTRING of string
  | YAMLSTRING of string
  | YAMLSQSTRING of string
  | YAMLDQSTRING of string
  | INDENT of int * int
  | DEDENT of int * int
  | NEWLINE (* internal token *)
  | EOF

type style_t =
    BLOCK of int
  | FLOW

let extract_indent_position = function
    (EOF, _) -> 0
  | (_, ({Lexing.pos_bol; pos_cnum; _}, _)) ->
    pos_cnum - pos_bol

module St = struct
type t =
  {
    lexbuf : Sedlexing.lexbuf
  ; mutable first_call : bool
  ; mutable style_stack : style_t list
  ; mutable at_bol : bool
  ; mutable pushback : (token * (Lexing.position * Lexing.position)) list
  }
  let mk lexbuf = {
    lexbuf
  ; first_call = true
  ; style_stack = [BLOCK 0]
  ; at_bol = true
  ; pushback = []
  }
  let set_bol st b = st.at_bol <- b
  let pop_flow st =
    match st with
      { style_stack = FLOW :: sst ; _ } -> st.style_stack <- sst
    | _ -> failwith "pop_flow: internal error"

  let push_flow st = st.style_stack <- FLOW::st.style_stack

let rec pop_styles0 loc rev_pushback = function
    ((BLOCK m)::(BLOCK m')::sst, n) when n < m -> pop_styles0 loc ((DEDENT(m',m),loc)::rev_pushback) ((BLOCK m')::sst, n)
  | ((BLOCK m)::sst, n) when n < m -> pop_styles0 loc ((DEDENT(n,m),loc)::rev_pushback) (sst, n)

  | ((BLOCK m)::sst, n) when n = m && m > -1 -> (rev_pushback, (BLOCK m)::sst)

  | ((BLOCK m)::sst, n) when n = m && m = -1 ->
    assert (sst = []) ;
    (rev_pushback, [BLOCK 0])
  | _ -> failwith "pop_styles: dedent did not move back to previous indent position"

let pop_styles a b c = pop_styles0 a b c

let handle_indents_with st ((tok,(spos,epos as loc)) as t) =
  assert (st.pushback = []) ;
  match st.style_stack with
    (BLOCK m)::_ ->
    let n = extract_indent_position t in
    if n = m then begin
      if tok = DASH then begin
        st.style_stack <- (BLOCK (n+1))::st.style_stack ;
        st.pushback <- [(INDENT(n,n+1),(spos,epos))];
        t
      end
      else
        t
    end
    else if n > m then begin
      if tok = DASH then begin
        st.style_stack <- (BLOCK (n+1))::(BLOCK n)::st.style_stack ;
        st.pushback <- [t; (INDENT(n,n+1),(spos,epos))];
        (INDENT(m,n),(spos,epos))
      end
      else begin
        st.style_stack <- (BLOCK n)::st.style_stack ;
        st.pushback <- [t] ;
        (INDENT(m,n),(spos,epos))
      end
    end
    else (* n < m *) begin
      let (rev_pushback, new_sst) = pop_styles loc [] (st.style_stack, n) in
      let new_pushback = (List.rev rev_pushback)@[t] in
      st.pushback <- List.tl new_pushback ;
      st.style_stack <- new_sst ;
      List.hd new_pushback
    end

let increment_indent_with ?(by=1) st ((tok,(spos,epos as loc)) as t) =
  assert (st.pushback = []) ;
  match st.style_stack with
    (BLOCK m)::_ ->
    st.style_stack <- (BLOCK (m + by))::st.style_stack ;
    st.pushback <- [(INDENT(m,m + by), (spos, epos))] ;
    t
  | _ -> failwith "increment_indent_with: should never be called in flow style"

end

let versiontag buf =
  match%sedlex buf with
    "%BS4J-1.0", Opt '\r', '\n' -> Some (Sedlexing.Latin1.lexeme buf, Sedlexing.lexing_positions buf)
  | _ -> None

let indentspaces buf =
  match%sedlex buf with
  | Star ' ' -> String.length (Sedlexing.Latin1.lexeme buf)
  | Star ' ', '\t' -> failwith "indentspaces: a <TAB> found at margin-indent"
  | _ -> failwith "indentspaces: should never happen"

let rec rawstring2 ((spos : Lexing.position), (id : Uchar.t array), (acc : Buffer.t)) st =
  let open St in
  let pos() = Sedlexing.lexing_positions st.lexbuf in
  let buf = st.lexbuf in
  match%sedlex buf with
  | Plus(Compl(')')) ->
    let txt = Sedlexing.Latin1.lexeme buf in
    Buffer.add_string acc txt ;
    rawstring2 (spos, id, acc) st
  | ")" ->
    Buffer.add_string acc ")" ;
    rawstring3 (spos, id, acc) 0 st
  | _ -> failwith "rawstring2: unexpected character"

and rawstring3 (spos, id, acc) ofs st =
  let open St in
  let pos() = Sedlexing.lexing_positions st.lexbuf in
  let buf = st.lexbuf in
  if ofs < Array.length id then
    match%sedlex buf with
    | alphanum ->
      let c = Sedlexing.lexeme_char buf 0 in
      Buffer.add_utf_8_uchar acc c ;
      if c = id.(ofs) then
        rawstring3 (spos, id, acc) (ofs+1) st
      else
        rawstring2 (spos, id, acc) st
    | _ -> rawstring2 (spos, id, acc) st
  else
    match%sedlex buf with
    | '"' ->
      Buffer.add_char acc '"' ;
      let (_, epos) = pos() in
      (RAWSTRING (Buffer.contents acc), (spos, epos))
    | _ -> rawstring2 (spos, id, acc) st


let rawstring1 (spos, id,acc) st =
  let open St in
  let pos() = Sedlexing.lexing_positions st.lexbuf in
  let buf = st.lexbuf in
  match%sedlex buf with
  | "(" ->
    Buffer.add_string acc "(" ;
    rawstring2 (spos, id, acc) st
  | _ -> failwith "rawstring1: unexpected character"


let rawstring0 spos st =
  let open St in
  let buf = st.lexbuf in
  match%sedlex buf with
  | Opt ident ->
    let uni_id = Sedlexing.lexeme buf in
    let id = Sedlexing.Latin1.lexeme buf in
    let acc = Buffer.create 23 in
    Buffer.add_string acc "R\"" ;
    Buffer.add_string acc id ;
    rawstring1 (spos, uni_id,acc) st
  | _ -> failwith "rawstring0: unexpected character"

let rec rawtoken st =
  let open St in
  let pos() = Sedlexing.lexing_positions st.lexbuf in
  let buf = st.lexbuf in
  match%sedlex buf with
  | decimal_float -> (DECIMAL (Sedlexing.Latin1.lexeme buf),pos())
  | hexadecimal_integer -> (HEXADECIMAL (Sedlexing.Latin1.lexeme buf),pos())
  | octal_integer -> (OCTAL (Sedlexing.Latin1.lexeme buf),pos())
  | json_string -> (JSONSTRING (Sedlexing.Latin1.lexeme buf),pos())
  | yaml_sqstring -> (YAMLSQSTRING (Sedlexing.Latin1.lexeme buf),pos())
  | yaml_dqstring -> (YAMLDQSTRING (Sedlexing.Latin1.lexeme buf),pos())
  | "R\"" ->
    let (spos, _) = pos() in
    rawstring0 spos st
  | "[" -> (LBRACKET, pos())
  | "]" -> (RBRACKET, pos())
  | "{" -> (LBRACE,pos())
  | "}" -> (RBRACE,pos())
  | ":" -> (COLON,pos())
  | "|" -> (BAR,pos())
  | "|-" -> (BARDASH,pos())
  | "|+" -> (BARPLUS,pos())
  | ">" -> (GT,pos())
  | ">-" -> (GTDASH,pos())
  | ">+" -> (GTPLUS,pos())
  | "," -> (COMMA,pos())
  | "-" -> (DASH,pos())
  | "---" -> (DASHDASHDASH,pos())
  | "..." -> (DOTDOTDOT,pos())
  | Plus linews -> rawtoken st
  | '\n' -> (NEWLINE,pos())
  | yamlscalar -> (YAMLSTRING (Sedlexing.Latin1.lexeme buf), pos())
  | eof -> (EOF,pos())
  | comment -> rawtoken st
  | _ -> failwith "Unexpected character"


let rec jsontoken0 st =
  let open St in
  match st with
    { first_call = true ; _ } -> begin
      st.first_call <- false ;
      match versiontag st.lexbuf with
        None -> jsontoken0 st
      | Some (s,p) -> (BS4J s,p)
    end
  | { pushback = h::t ; _ } ->
    st.pushback <- t ;
    h

  | { pushback = [] ; at_bol = true ; style_stack = (BLOCK _) :: _ ; _ } ->
    ignore(indentspaces st.lexbuf) ;
    St.set_bol st false ;
    jsontoken0 st

  | { pushback = [] ; at_bol = false ; style_stack = (BLOCK m) :: sst ; _ } -> begin
      match rawtoken st with
        (RBRACKET, _) -> failwith "jsontoken: ']' found in block style"
      | (LBRACKET, _) as t -> St.push_flow st ; t
      | (RBRACE, _) -> failwith "jsontoken: '}' found in block style"
      | (LBRACE, _) as t -> St.push_flow st ; t
      | (COLON, _) as t -> increment_indent_with st t

      | ((GT|GTDASH|GTPLUS), _) as t -> t
      | ((BAR|BARDASH|BARPLUS), _) as t -> t

      | (NEWLINE, _) -> St.set_bol st true ; jsontoken0 st
      | t -> handle_indents_with st t
  end

  | { pushback = [] ; style_stack = FLOW :: _ ; _ } -> begin
      match rawtoken st with
        (RBRACKET, _) as t -> St.pop_flow st ; t
      | (LBRACKET, _) as t -> St.push_flow st ; t
      | (RBRACE, _) as t -> St. pop_flow st ; t
      | (LBRACE, _) as t -> St.push_flow st ; t
      | (NEWLINE, _) -> St.set_bol st true ; jsontoken0 st
      | t -> t
    end

let jsontoken st = jsontoken0 st

let ocamllex_string s =
  let st = St.mk (Sedlexing.Latin1.from_gen (gen_of_string s)) in
  let rec lexrec acc =
    match jsontoken st with
      (EOF,_) as t -> List.rev (t::acc)
    | t -> lexrec (t::acc)
  in lexrec []

let lex1 f s =
  let lb = Sedlexing.Latin1.from_gen (gen_of_string s) in
  f lb

let lex2 f s =
  let lb = Sedlexing.Latin1.from_gen (gen_of_string s) in
  let st = St.mk lb in
  f st
