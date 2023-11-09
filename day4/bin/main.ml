module Range : sig
  type t

  val ( -- ) : int -> int -> t
  val contains_range : t -> t -> bool
end = struct
  type t = int * int

  let ( -- ) from to_ = (from, to_)
  let start (from, _) = from
  let end_ (_, to_) = to_
  let contains n (from, to_) = from <= n && to_ >= n

  let contains_range smaller bigger =
    bigger |> contains (smaller |> start) && bigger |> contains (smaller |> end_)
end

let print_bool b =
  b |> string_of_bool |> print_string;
  print_newline ()

let () =
  let open Range in
  let a = 2 -- 4 |> contains_range (6 -- 8) in
  let b = 6 -- 8 |> contains_range (2 -- 4) in
  let c = 4 -- 6 |> contains_range (6 -- 6) in
  print_bool a;
  print_bool b;
  print_bool c
