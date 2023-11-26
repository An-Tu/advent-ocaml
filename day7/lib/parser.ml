type command = LS | CD of string
type entry = Dir of string | File of (int * string)
type t = Command of command | Entry of entry

let to_string = function
  | Command LS -> "Command: LS"
  | Command (CD path) -> Printf.sprintf "Command: CD { path: %s }" path
  | Entry (Dir dir_name) -> Printf.sprintf "Directory { name: %s }" dir_name
  | Entry (File (size, file_name)) ->
      Printf.sprintf "File { name: %s, size: %d }" file_name size

let is_path_char c = match c with 'a' .. 'z' | '.' | '/' -> true | _ -> false
let is_digit c = match c with '0' .. '9' -> true | _ -> false

let integer =
  let open Angstrom in
  take_while1 is_digit >>| int_of_string

let path =
  let open Angstrom in
  take_while1 is_path_char

let ls =
  let open Angstrom in
  string "ls" >>| fun _ -> LS

let cd =
  let open Angstrom in
  string "cd " *> path >>| fun path -> CD path

let command =
  let open Angstrom in
  string "$ " *> (ls <|> cd) >>| fun res -> Command res

let dir =
  let open Angstrom in
  string "dir " *> path >>| fun path -> Dir path

let file =
  let open Angstrom in
  integer >>= fun size ->
  char ' ' >>= fun _ ->
  path >>= fun path -> return (File (size, path))

let entry =
  let open Angstrom in
  dir <|> file >>| fun res -> Entry res

let line =
  let open Angstrom in
  command <|> entry <* (end_of_line <|> end_of_input)

let parse_all =
  let open Angstrom in
  many line
