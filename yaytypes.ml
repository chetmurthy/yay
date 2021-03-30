

type yaml =
  [
    `Null
  | `Bool of bool
  | `Float of (float [@equal fun x y -> 0 = compare x y])
  | `String of string
  | `A of yaml list
  | `O of (string * yaml) list
  ] [@@deriving show,eq]

type yaml_list = yaml list [@@deriving (show,eq)]

type json =
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Float of float
    | `String of string
    | `Assoc of (string * json) list
    | `List of json list
    ] [@@deriving show,eq]

type json_list = json list [@@deriving (show,eq)]

let yaml2json (y : yaml) : json =
  let rec yrec = function
    `Null -> `Null
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `String s -> `String s
  | `A l -> `List (List.map yrec l)
  | `O l -> `Assoc (List.map (fun (k,v) -> (k,yrec v)) l)
  in yrec y

let canon_yaml (y : yaml) : yaml =
  let rec yrec = function
    `Null -> `Null
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `String s -> `String s
  | `A l -> `A (List.map yrec l)
  | `O l -> `O (List.stable_sort Stdlib.compare (List.map (fun (k,v) -> (k,yrec v)) l))
  in yrec y

let json2yaml (y : json) : yaml =
  let rec yrec = function
    `Null -> `Null
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Int n -> `Float (float_of_int n)
  | `String s -> `String s
  | `List l -> `A (List.map yrec l)
  | `Assoc l -> `O (List.map (fun (k,v) -> (k,yrec v)) l)
  in yrec y

let canon_json (y : json) : json =
  let rec yrec = function
    `Null -> `Null
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Int n -> `Float (float_of_int n)
  | `String s -> `String s
  | `List l -> `List (List.map yrec l)
  | `Assoc l -> `Assoc (List.stable_sort Stdlib.compare (List.map (fun (k,v) -> (k,yrec v)) l))
  in yrec y

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
  | EOF [@@deriving show { with_path = false},eq]
