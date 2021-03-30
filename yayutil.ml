
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
