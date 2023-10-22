let read_lines_seq file =
  let ic = open_in file in
  let was_closed = ref false in
  let rec loop () () =
    match !was_closed with
    | true -> Seq.Nil
    | false -> (
        try
          Seq.Cons
            ((match input_line ic with "" -> None | l -> Some l), loop ())
        with End_of_file ->
          close_in ic;
          was_closed := true;
          Seq.Nil)
  in
  loop ()

let read_lines_iter file =
  let ic = open_in file in
  let try_read () =
    try
      let line = input_line ic in
      let r = if line = "" then None else Some line in
      Some r
    with End_of_file ->
      close_in ic;
      None
  in
  Iter.from_fun try_read
