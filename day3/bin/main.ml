module Item : sig
  type t

  val pp : Format.formatter -> t -> unit
  val from_char : char -> (t, string) result
  val priority : t -> int
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

let pp_print_array_items fmt items =
  Format.fprintf fmt "%a"
    (Lib.Print.pp_print_array
       (Format.pp_print_result ~ok:Item.pp ~error:Format.pp_print_string))
    items

let pp_print_array_items_2 fmt items =
  Format.fprintf fmt "%a" (Lib.Print.pp_print_array Item.pp) items

module ItemSet = Set.Make (struct
  type t = Item.t

  let compare = compare
end)

let sum =
  Lib.Files.read_lines_iter "./input.txt"
  |> Iter.filter_map
       (Option.map (fun line ->
            line |> Lib.Helpers.string_to_array_chars
            |> Array.map Item.from_char))
  |> Iter.map (fun items ->
         let l = Array.length items in
         let half = l / 2 in
         let first = ref ItemSet.empty in
         for i = 0 to half - 1 do
           match Array.get items i with
           | Error _ -> ()
           | Ok item -> first := ItemSet.add item !first
         done;
         let second = Array.sub items half half in

         let priority =
           second
           |> Array.find_map (function
                | Error _ -> None
                | Ok item ->
                    if ItemSet.mem item !first then Some (Item.priority item)
                    else None)
         in
         match priority with
         | None ->
             raise (Invalid_argument "there should be at least one duplicate")
         | Some p ->
             Printf.printf "priority: %d\n" p;
             p)
  |> Iter.sum
;;

Printf.printf "sum: %d\n" sum
