open OUnit2
open OUnitTest
open Pa_ppx_testutils

let testsuite_dir = "JSONTestSuite"

let skiplist = [
  ("n_structure_trailing_#.json", Some "BS4J allows perl comments in JSON mode")
; ("n_object_with_trailing_garbage.json", Some "BS4J allows perl comments in JSON mode")
; ("n_object_trailing_comment_slash_open.json", Some "BS4J allows c++ comments in JSON mode")
; ("n_object_trailing_comment.json", Some "BS4J allows c comments in JSON mode")
; ("n_structure_object_with_comment.json", Some "BS4J allows c comments in JSON mode")
; ("i_number_very_big_negative_int.json", Some "yojson fails with integer underflow")
; ("i_number_too_big_neg_int.json", Some "yojson fails with integer underflow")
; ("i_string_UTF-16LE_with_BOM.json", Some "UTF-16LE codepage")
; ("i_number_too_big_pos_int.json", Some "yojson fails with integer overflow")
; ("i_structure_UTF-8_BOM_empty_object.json", Some "UTF-8 with BOM")
; ("/home/chet/Hack/Github/nst/JSONTestSuite/test_parsing/i_string_utf16BE_no_BOM.json", Some "UTF-16BE codepage")
; ("i_string_utf16BE_no_BOM.json", Some "UTF-16BE codepage")
; ("i_string_utf16LE_no_BOM.json", Some "UTF-16LE codepage")
]

let errorlist = [
  "i_string_invalid_surrogate.json"
; "i_string_1st_surrogate_but_2nd_missing.json"
; "i_string_inverted_surrogates_U+1D11E.json"
; "i_string_invalid_lonely_surrogate.json"
; "i_string_1st_valid_surrogate_2nd_invalid.json"
; "i_string_incomplete_surrogates_escape_valid.json"
; "i_string_incomplete_surrogate_and_escape_valid.json"
]

module JSONTestsuite = struct

let printer x = Fmt.(str "%a" Jsontypes.pp_yaml x)
let cmp = Jsontypes.equal_yaml

let of_string_exn s =
  s
  |> Jsonparse.(parse_string parse_json_eoi)
  |> Jsontypes.json2yaml


let yojson_of_string_exn jsons =
  Jsontypes.json2yaml (Yojson.Basic.from_string jsons)

let file_contents fname =
  fname
  |> Fpath.v
  |> Bos.OS.File.read
  |> Rresult.R.get_ok

let assert_raises_exn_pattern pattern f =
  let open Tml in
  Testutil.assert_raises_exn_pred
    (function
        Failure msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Stdlib.Stream.Error msg) when matches ~pattern msg -> true
      | Stdlib.Stream.Error msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Failure msg) when matches ~pattern msg -> true
      | Invalid_argument msg when matches ~pattern msg -> true
      | Ploc.Exc(_, Invalid_argument msg) when matches ~pattern msg -> true
      | Stdlib.Stack_overflow -> true
      | Ploc.Exc(_, Stdlib.Stack_overflow) -> true
      | _ -> false
    )
    f


let exec ok fname =
  let jsons = file_contents fname in
  if ok then
    let expect = Jsontypes.canon_yaml (yojson_of_string_exn jsons) in
    let got = Jsontypes.canon_yaml (of_string_exn jsons) in
    assert_equal ~printer
      expect
      got
  else
    assert_raises_exn_pattern ""
      (fun () -> Jsontypes.canon_yaml (of_string_exn jsons))

end

let make_test fname =
  let open Tml in
  let base = Fpath.(fname |> v |> basename) in

  match List.assoc base skiplist with
    msg ->
    let msg = match msg with None -> base | Some s -> s in
    Tml.warning (Fmt.str "%s: Not handled: %s" fname msg) ;
    let name = Fmt.(str "[ SKIP %s ]" fname) in
    base >::: [
      name >:: (fun ctxt ->
          Tml.warning (Fmt.str "%s: Not handled: %s" fname msg)
        )
    ]
  | exception Not_found -> begin
      match String.get base 0 with
      'y' -> 
        let name = Fmt.(str "[ OK %s ]" fname) in
        base >::: [
          name >:: (fun ctxt ->
              JSONTestsuite.exec true fname
            )
        ]
      | 'n' ->
        let name = Fmt.(str "[ ERROR %s ]" fname) in
        base >::: [
          name >:: (fun ctxt ->
              JSONTestsuite.exec false fname
            )
        ]
      | 'i' ->
        if List.mem base errorlist then
          let name = Fmt.(str "[ INDETERMINATE->ERROR %s ]" fname) in
          base >::: [
            name >:: (fun ctxt ->
                JSONTestsuite.exec false fname
              )
          ]
        else
          let name = Fmt.(str "[ INDETERMINATE->OK %s ]" fname) in
          base >::: [
            name >:: (fun ctxt ->
                JSONTestsuite.exec true fname
              )
          ]
      | _ ->
        failwith Fmt.(str "%s: unhandled case in JSONTestsuite.exec" fname)
    end

let exec1 fname =
  JSONTestsuite.exec fname

let tests = "JSONTestsuite" >::: (
    let fl = 
      Fpath.(add_seg (testsuite_dir |> v) "test_parsing")
      |> Bos.OS.Dir.contents
      |> Rresult.R.get_ok
      |> List.map Fpath.to_string in
    List.map make_test fl
  )

if not !Sys.interactive then
  run_test_tt_main tests
;;
