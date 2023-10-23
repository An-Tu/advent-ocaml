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
