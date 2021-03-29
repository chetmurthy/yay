open OUnit2
open OUnitTest
open Pa_ppx_testutils

let warning s = Fmt.(pf stderr "%s\n%!" s)

let list_of_stream strm =
let rec listrec acc = parser
  [< 't ; strm >] -> listrec (t::acc) strm
| [< >] -> List.rev acc
in listrec [] strm

let tml_re = Str.regexp ".*\\.tml$"

let is_tml f =
  Str.string_match tml_re f 0

let files ?override_dir dir =
  let l =  dir
           |> Fpath.v
           |> Bos.OS.Dir.contents ~rel:false
           |> Rresult.R.get_ok
           |> List.map Fpath.to_string in
  let overrides =  match override_dir with None -> [] | Some d ->
    d
    |> Fpath.v
    |> Bos.OS.Dir.contents ~rel:false
    |> Rresult.R.get_ok
    |> List.map Fpath.to_string in
  List.filter is_tml (l@overrides)

type t =
  {
    name : string
  ; filename : string
  ; sections : (string * string list) list
  }

let spec_line = Str.regexp "=== \\(.*\\)"
let sect_line = Str.regexp "--- \\([^:]*\\)"

let match_extract rex groupl s =
  if not (Str.string_match rex s 0) then
    None
  else Some(List.map (fun n -> Str.matched_group n s) groupl)

let parse_lines ?filename l =
  let filename = match filename with None -> "" | Some s -> s in
  let (specl, sectl1, tl) = match l with
      (specl :: sectl1 :: tl) -> (specl, sectl1, tl)
    | _ -> failwith "Tml.mk: need at least two lines"
  in
  match match_extract spec_line [1] specl with
    None -> failwith "Tml.mk: failed to match spec line"
  | Some [name] ->
    let rec sectrec acc (sectname,sectacc) = function
        [] -> List.rev ((sectname, List.rev sectacc)::acc)
      | h::t -> begin match match_extract sect_line [1] h with
            None -> sectrec acc (sectname, h::sectacc) t
          | Some [name] -> sectrec ((sectname, List.rev sectacc)::acc) (name, [h]) t
          | _ -> assert false
        end
    in begin
      match match_extract sect_line [1] sectl1 with
        None -> failwith "Tml.mk: failed to match first section line"
      | Some [sectname] ->
        { name = name
        ; filename = filename
        ; sections = sectrec [] (sectname, [sectl1]) tl }
      | _ -> assert false
    end
  | _ -> assert false

let from_string s =
  let l = String.split_on_char '\n' s in
  parse_lines l

let read_lines ic =
  let rec rerec acc =
    match Stdlib.input_line ic with
      s -> rerec (s::acc)
    | exception End_of_file -> List.rev acc
  in rerec []

let from_channel ic =
  let l = read_lines ic in
  parse_lines l

let from_file f =
  let l = f |> Fpath.v |> Bos.OS.File.read_lines
          |> Rresult.R.get_ok in
  parse_lines ~filename:f l


let find_sect t sname =
  match List.assoc sname t.sections with
    l -> Some l
  | exception Not_found -> None

let tags_line_re = Str.regexp "--- tags: *\\(.*\\)"

let tags t =
  let line = match List.assoc "tags" t.sections with
      line::_ -> line
    | exception Not_found -> failwith (Fmt.(str "%s: missing tags line" t.filename)) in
  match match_extract tags_line_re [1] line with
    None -> failwith (Fmt.(str "%s: missing malformed line" t.filename))
  | Some [s] -> String.split_on_char ' ' s
  | _ -> assert false

let matches ~pattern text =
  match Str.search_forward (Str.regexp pattern) text 0 with
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

let tab_re = Str.regexp "<TAB>"
let spc_re = Str.regexp "<SPC>"

let perform_subst s =
  let s = Str.(global_substitute tab_re (fun _ -> "\t") s) in
  let s = Str.(global_substitute spc_re (fun _ -> " ") s) in
  s

let is_comment s = String.length s > 0 && String.get s 0 = '#'

let strip_comments l =
  List.filter (fun s -> not (is_comment s)) l

let extract_yaml t = function
    (("in-yaml"|"out-yaml"), l) ->
    l
    |> List.tl
    |> String.concat "\n"
    |> perform_subst

  | (("in-yaml(<)"|"in-yaml(<+)"|"out-yaml(<)"|"out-yaml(<+)"), l) ->
    l
    |> List.tl
    |> strip_comments
    |> List.map (consume_indent 4)
    |> String.concat "\n"
    |> perform_subst

  | (("in-yaml(+)"|"out-yaml(+)"), l) ->
    l
    |> List.tl
    |> String.concat "\n"
    |> perform_subst

  | _ -> failwith (Fmt.(str "%s: internal error in extract_yaml" t.filename))

let find_yaml t sectname =
  match (find_sect t sectname
        ,find_sect t (sectname^"(<)")
        ,find_sect t (sectname^"(+)")
        ,find_sect t (sectname^"(<+)")
        ) with
    (Some x, None, None, None) -> Some (sectname, x)
  | (None, Some x, None, None) -> Some (sectname^"(<)", x)
  | (None, None, Some x, None) -> Some (sectname^"(+)", x)
  | (None, None, None, Some x) -> Some (sectname^"(<+)", x)
  | (None, None, None, None) -> None
  | _ ->
    failwith (Fmt.(str "%s: malformed YAML sections" t.filename))

module OCamlYAML = struct

let printer x = Fmt.(str "%a" Jsontypes.pp_yaml_list x)
let cmp = Jsontypes.equal_yaml_list

let parse_yaml t =
  match find_yaml t "in-yaml" with
    Some yamlp ->
    let yamls = extract_yaml t yamlp in
      (Jsontypes.canon_yaml (Yaml.of_string_exn yamls))
  | None -> failwith (Fmt.(str "%s: no YAML found" t.filename))

let exec t =
  match (find_yaml t "in-yaml"
        ,find_sect t "in-json"
        ,find_yaml t "out-yaml"
        ,find_sect t "error"
        )
  with
    (Some yamlp
    ,Some jsonl, _, None) ->
    let yamls = extract_yaml t yamlp in
    let jsons = String.concat "\n" (List.tl jsonl) in
    assert_equal ~printer
      (List.map Jsontypes.canon_yaml (List.map Jsontypes.json2yaml (list_of_stream (Yojson.Basic.stream_from_string jsons))))
      [(Jsontypes.canon_yaml (Yaml.of_string_exn yamls))]

  | (Some inyamlp
    ,None
    ,Some outyamlp
    , None) ->
    let inyamls = extract_yaml t inyamlp in
    let outyamls = extract_yaml t outyamlp in
    assert_equal ~printer
      [(Yaml.of_string_exn outyamls)]
      [(Yaml.of_string_exn inyamls)]

  | (Some yamlp
    ,_, _, Some errorl) ->
    let yamls = extract_yaml t yamlp in
    assert_raises_exn_pattern
      ""
      (fun () -> (Yaml.of_string_exn yamls))

  | (Some _
    ,None, None, None) ->
    warning Fmt.(str "%s: test not meant to be executed (I guess)" t.filename)

  | _ -> failwith (Fmt.(str "%s: unhandled TML syntax" t.filename))

end

module BS4J = struct

let printer x = Fmt.(str "%a" Jsontypes.pp_yaml_list x)
let cmp = Jsontypes.equal_yaml_list

let docs_of_string_exn s =
  s
  |> Jsonparse.(parse_string parse_docs_eoi)
  |> List.map Jsontypes.json2yaml

let parse_yaml t =
  match find_yaml t "in-yaml" with
    Some yamlp ->
    let yamls = extract_yaml t yamlp in
      (List.map Jsontypes.canon_yaml (docs_of_string_exn yamls))
  | None -> failwith (Fmt.(str "%s: no YAML found" t.filename))

let exec t =
  match (find_yaml t "in-yaml"
        ,find_sect t "in-json"
        ,find_yaml t "out-yaml"
        ,find_sect t "error"
        )
  with
    (Some yamlp
    ,Some jsonl, _, None) ->
    let yamls = extract_yaml t yamlp in
    let jsons = String.concat "\n" (List.tl jsonl) in
    assert_equal ~printer
      (List.map Jsontypes.canon_yaml (List.map Jsontypes.json2yaml (list_of_stream (Yojson.Basic.stream_from_string jsons))))
      (List.map Jsontypes.canon_yaml (docs_of_string_exn yamls))

  | (Some inyamlp
    ,None
    ,Some outyamlp
    , None) ->
    let inyamls = extract_yaml t inyamlp in
    let outyamls = extract_yaml t outyamlp in
    assert_equal ~printer
      [(Yaml.of_string_exn outyamls)]
      (List.map Jsontypes.canon_yaml (docs_of_string_exn inyamls))

  | (Some yamlp
    ,_, _, Some errorl) ->
    let yamls = extract_yaml t yamlp in
    assert_raises_exn_pattern
      ""
      (fun () -> List.map Jsontypes.canon_yaml (docs_of_string_exn yamls))

  | (Some _
    ,None, None, None) ->
    warning Fmt.(str "%s: test not meant to be executed (I guess)" t.filename)

  | _ -> failwith (Fmt.(str "%s: unhandled TML syntax" t.filename))

end

module JSON = struct

let printer x = Fmt.(str "%a" Jsontypes.pp_yaml_list x)
let cmp = Jsontypes.equal_yaml_list

let of_string_exn s =
  s
  |> Jsonparse.(parse_string parse_flow_json_stream_eoi)
  |> List.map Jsontypes.json2yaml

let parse_json t =
  match find_sect t "in-json" with
    Some jsonl ->
    let jsons = String.concat "\n" (List.tl jsonl) in
      (List.map Jsontypes.canon_yaml (of_string_exn jsons))
  | None -> failwith (Fmt.(str "%s: no JSON found" t.filename))

let exec t =
  match find_sect t "in-json" with
    Some jsonl ->
    let jsons = String.concat "\n" (List.tl jsonl) in
    assert_equal ~printer
      (List.map Jsontypes.canon_yaml (List.map Jsontypes.json2yaml (list_of_stream (Yojson.Basic.stream_from_string jsons))))
      (List.map Jsontypes.canon_yaml (of_string_exn jsons))

  | None ->
    warning Fmt.(str "%s: no JSON to test" t.filename)

end
