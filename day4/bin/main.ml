module Range : sig
  type t

  val ( -- ) : int -> int -> t
  val contains_or_is_contained : t -> t -> bool
end = struct
  type t = int * int

  let ( -- ) from to_ = (from, to_)
  let start (from, _) = from
  let end_ (_, to_) = to_
  let contains n (from, to_) = from <= n && to_ >= n

  let contains_range smaller bigger =
    bigger |> contains (smaller |> start) && bigger |> contains (smaller |> end_)

  let contains_or_is_contained r1 r2 =
    r1 |> contains_range r2 || r2 |> contains_range r1
end

exception Invalid_Params of string

let redundant =
  let open Range in
  Lib.Files.read_lines_iter "./input.txt"
  |> Iter.filter_map
       (Option.map (fun l ->
            match
              l |> String.split_on_char ','
              |> List.map (fun range ->
                     match
                       range |> String.split_on_char '-'
                       |> List.map int_of_string
                     with
                     | [ start; end_ ] -> start -- end_
                     | _ ->
                         raise
                           (Invalid_Params
                              "each range should have a start and end"))
            with
            | [ r1; r2 ] -> (r1, r2)
            | _ -> raise (Invalid_Params "each line must have a pair of ranges")))
  |> Iter.filter_count (fun (r1, r2) -> r1 |> Range.contains_or_is_contained r2)
;;

print_int redundant;
print_newline ()
