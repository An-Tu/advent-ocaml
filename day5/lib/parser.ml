type line_type =
  | Crate_line of string option list
  | Instruction of (int * int * int)
  | Unknown

let to_string = function
  | Crate_line _ -> "Crate line"
  | Instruction _ -> "Instruction"
  | Unknown -> "Unknown"

let is_whitespace = function
  | '\x20' | '\x0a' | '\x0d' | '\x09' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false
let hole_str = "   "

let integer =
  let open Angstrom in
  take_while is_digit >>| int_of_string

let whitespace =
  let open Angstrom in
  map any_char ~f:is_whitespace >>= function
  | true -> return ()
  | false -> fail "whitespace expected"

let hole =
  let open Angstrom in
  string hole_str

let crate =
  let open Angstrom in
  char '[' *> take 1 <* char ']'

let crate_hole =
  let open Angstrom in
  map (crate <|> hole) ~f:(function
    | res when res = hole_str -> None
    | res -> Some res)

let whitespace_crate =
  let open Angstrom in
  crate_hole <* (whitespace <|> end_of_input)

let line_of_crates =
  let open Angstrom in
  many1 whitespace_crate >>| fun data -> Crate_line data

let line_of_instruction =
  let open Angstrom in
  string "move " *> integer <* whitespace >>= fun quantity ->
  string "from " *> integer <* whitespace >>= fun src ->
  string "to " *> integer >>= fun dst ->
  return (Instruction (quantity, src, dst))

let parse_line =
  let open Angstrom in
  line_of_crates <|> line_of_instruction <|> return Unknown
