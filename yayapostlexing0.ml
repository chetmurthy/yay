open Jsontypes
open Yayalexing

module Final = struct

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

let rec jsontoken0 st =
  let open St in
  match st with
    { first_call = true ; _ } -> begin
      st.first_call <- false ;
      match Lexers.versiontag st.lexbuf with
        None -> jsontoken0 st
      | Some (s,p) -> (BS4J s,p)
    end
  | { pushback = h::t ; _ } ->
    st.pushback <- t ;
    h

  | { pushback = [] ; at_bol = true ; style_stack = (BLOCK _) :: _ ; _ } ->
    ignore(Lexers.indentspaces st.lexbuf) ;
    St.set_bol st false ;
    jsontoken0 st

  | { pushback = [] ; at_bol = false ; style_stack = (BLOCK m) :: sst ; _ } -> begin
      match Lexers.rawtoken st.lexbuf with
        (RBRACKET, _) -> failwith "jsontoken: ']' found in block style"
      | (LBRACKET, _) as t -> St.push_flow st ; t
      | (RBRACE, _) -> failwith "jsontoken: '}' found in block style"
      | (LBRACE, _) as t -> St.push_flow st ; t
      | (COLON, _) as t -> increment_indent_with st t
      | ((YAMLSTRING _|YAMLSQSTRING _|YAMLDQSTRING _|RAWSTRING _
         |DECIMAL _|HEXADECIMAL _|OCTAL _
         |DASH|DASHDASHDASH|DOTDOTDOT|EOF), _) as t -> handle_indents_with st t
      | (NEWLINE, _) -> St.set_bol st true ; jsontoken0 st

      | t -> t
  end

  | { pushback = [] ; style_stack = FLOW :: _ ; _ } -> begin
      match Lexers.rawtoken st.lexbuf with
        (RBRACKET, _) as t -> St.pop_flow st ; t
      | (LBRACKET, _) as t -> St.push_flow st ; t
      | (RBRACE, _) as t -> St. pop_flow st ; t
      | (LBRACE, _) as t -> St.push_flow st ; t
      | (NEWLINE, _) -> St.set_bol st true ; jsontoken0 st
      | t -> t
    end

let jsontoken st = jsontoken0 st

let tokenize lb =
  let st = St.mk lb in
  fun () -> jsontoken st

let ocamllex_string s =
  let tokf = tokenize (Sedlexing.Latin1.from_gen (gen_of_string s)) in
  let rec lexrec acc =
    match tokf () with
      (EOF,_) as t -> List.rev (t::acc)
    | t -> lexrec (t::acc)
  in lexrec []

end
