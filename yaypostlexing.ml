open Yaytypes
open Yaylexing

module Wellformed = struct

type state_t = { at_start : bool ; at_bol : bool ; flow_depth : int }

let stream lexbuf =
  let rec strec st =
    match st with
      {at_start=true;_} -> begin
      match Lexers.versiontag lexbuf with
        Some (vs,p) -> [< '(BS4J vs,p) ; strec { st with at_start=false} >]
        | None -> strec { st with at_start=false}
    end

    | {at_start=false; at_bol=true ; flow_depth = 0} ->
      ignore (Lexers.indentspaces lexbuf) ;
      strec { st with at_bol=false }

    | { flow_depth ; _ } -> begin
        match Lexers.rawtoken lexbuf with
          (NEWLINE,_) -> strec { st with at_bol = true }
        | ((RBRACE|RBRACKET),_) as t ->
          if flow_depth = 0 then failwith "Wellformed.stream: ']' or '}' not in flow style"
          else [< 't ; strec { st with flow_depth = flow_depth - 1 } >]

        | ((LBRACE|LBRACKET),_) as t ->
          [< 't ; strec { st with flow_depth = flow_depth + 1 } >]

        | t -> [< 't ; strec st >]
      end
  in strec { at_start=true ; at_bol=true ; flow_depth = 0 }
end

module Indented = struct

  type style_t =
      BLOCK of int
    | FLOW

  let extract_indent_position = function
      (EOF, _) -> 0
    | (_, ({Lexing.pos_bol; pos_cnum; _}, _)) ->
      pos_cnum - pos_bol


  let rec pop_styles0 loc rev_pushback = function
      ((BLOCK m)::(BLOCK m')::sst, n) when n < m -> pop_styles0 loc ((DEDENT(m',m),loc)::rev_pushback) ((BLOCK m')::sst, n)
    | ((BLOCK m)::sst, n) when n < m -> pop_styles0 loc ((DEDENT(n,m),loc)::rev_pushback) (sst, n)

    | ((BLOCK m)::sst, n) when n = m && m > -1 -> (rev_pushback, (BLOCK m)::sst)

    | ((BLOCK m)::sst, n) when n = m && m = -1 ->
      assert (sst = []) ;
      (rev_pushback, [BLOCK 0])
    | _ -> failwith "pop_styles: dedent did not move back to previous indent position"

  let pop_styles a b c = pop_styles0 a b c

  let rec stream sst = parser
    [< '((RBRACKET|RBRACE), _) as t ; strm >] -> begin
      match sst with
        FLOW::sst -> [< 't ; stream sst strm >]
      | _ -> failwith "Indented.stream: `}' or `}' found at non-flow position"
    end

  | [< '((LBRACKET|LBRACE), _) as t ; strm >] ->
    [< 't ; stream (FLOW::sst) strm >]

  | [< '(COLON, loc) as t ; strm >] -> begin
      match sst with
        FLOW::_ -> [< 't ; stream sst strm >]
      | (BLOCK m)::_ -> [< 't ; '(INDENT(m, m+1), loc); stream ((BLOCK (m+1))::sst) strm >]
    end

  | [< '((YAMLSTRING _|YAMLSQSTRING _|YAMLDQSTRING _|RAWSTRING _
         |DECIMAL _|HEXADECIMAL _|OCTAL _
         |DASH|DASHDASHDASH|DOTDOTDOT|EOF), loc) as t ; strm >] -> begin
      match sst with
        (BLOCK m)::_ ->
        let n = extract_indent_position t in begin
          match (t, Stdlib.compare n m) with
            ((DASH, _), 0) ->
            [< 't ; '(INDENT(n,n+1), loc); stream (BLOCK (n+1)::sst) strm >]
          | ((DASH,_), 1) ->
            [< '(INDENT(m,n), loc) ; 't ; '(INDENT(n,n+1), loc) ; stream ((BLOCK (n+1))::(BLOCK n)::sst) strm >]

          | (_, 0) ->
            [< 't ; stream sst strm >]

          | (_, 1) ->
            [< '(INDENT(m,n), loc) ; 't ; stream ((BLOCK n)::sst) strm >]

          | ((_, loc), -1) ->
            let (rev_pushback, new_sst) = pop_styles loc [] (sst, n) in
            let new_pushback = List.rev (t::rev_pushback) in
            [< Stream.of_list new_pushback ; stream new_sst strm >]
        end

      | FLOW::_ ->
        [< 't ; stream sst strm >]

      | [] -> failwith "Indented.stream: Internal error: should never get here"
    end

  | [< 't ; strm >] -> [< 't ; stream sst strm >]
end

module Final = struct

let token_stream lb =
  let strm = Wellformed.stream lb in
  Indented.(stream [BLOCK 0] strm)

let tokenize lb =
  let strm = token_stream lb in
  fun () -> Stream.next strm

let of_string f s =
  let lb = Sedlexing.Latin1.from_gen (gen_of_string s) in
  f lb

let ocamllex_string s =
  let tokf = tokenize (Sedlexing.Latin1.from_gen (gen_of_string s)) in
  let rec lexrec acc =
    match tokf () with
      (EOF,_) as t -> List.rev (t::acc)
    | t -> lexrec (t::acc)
  in lexrec []

end
