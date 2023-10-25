module Item : sig
  type t

  val pp : Format.formatter -> t -> unit
  val from_char : char -> (t, string) result
  (* val priority : t -> int *)
end = struct
  type t = char

  let pp = Format.pp_print_char

  let from_char = function
    | 'a' .. 'z' as c -> Ok c
    | 'A' .. 'Z' as c -> Ok c
    | c -> Error (Char.escaped c ^ " is not valid item")

  let priority = function
    | 'a' .. 'z' as c -> 1 + (Char.code c - Char.code 'a')
    | 'A' .. 'Z' as c -> 27 + (Char.code c - Char.code 'A')
    | _ -> raise (Invalid_argument "unreachable")
end

let string_to_list s =
  let rec iter str i =
    if String.length str = i then [] else String.get str i :: iter str (i + 1)
  in
  iter s 0

let _a =
  Lib.Files.read_lines_iter "./input.txt"
  |> Iter.filter_map
       (Option.map (fun line ->
            line |> string_to_list |> List.map Item.from_char))
  |> Iter.iter (fun items ->

         Format.printf "[%a]@."
           (Format.pp_print_list
              ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ", ")
              (Format.pp_print_result ~ok:Item.pp ~error:Format.pp_print_string))
           items)
