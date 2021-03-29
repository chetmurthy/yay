open OUnit2
open OUnitTest

let tml_dir = "/home/chet/Hack/Github/yaml/yaml-test-suite/test"

let only_tags = []
let exclude_tags = []

let skiplist = []

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
          JSON.exec t
        )
    ]

let exec1 n =
  let file =
    if n |>  Fpath.v |> Bos.OS.File.exists |> Rresult.R.get_ok then n
    else Fpath.(to_string (append (v tml_dir) (v n))) in
  Tml.(JSON.exec (from_file file))

let tests = "JSON testsuite" >::: (
    let fl = Tml.files ~override_dir:"bs4j-overrides" tml_dir in
    let tests = List.map Tml.from_file fl in
    let tests = select_tests ~only_tags ~exclude_tags tests in
    List.map make_test tests
  )

if not !Sys.interactive then
  run_test_tt_main tests
;;
