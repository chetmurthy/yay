
let list_of_stream strm =
let rec listrec acc = parser
  [< 't ; strm >] -> listrec (t::acc) strm
| [< >] -> List.rev acc
in listrec [] strm

let list_of_stream_until pred strm =
let rec listrec acc = parser
  [< 't when pred t; strm >] -> List.rev (t::acc)
| [< 't ; strm >] -> listrec (t::acc) strm
| [< >] -> List.rev acc
in listrec [] strm

let failwith_loc loc s = Ploc.raise loc (Failure s)

(* borrowed from ounit *)
let failwithf fmt =
  Fmt.kstrf failwith fmt

let failwith_locf loc fmt =
  Fmt.kstrf (failwith_loc loc) fmt

(* shadow "failwith" in the Fmt module, so that the form

  Fmt.(failwith ....)

will be a type error (since it almost certainly should have been
   "failwithf"

*)
module Fmt = struct
  include Fmt
  let failwith = ()
end
