let () =
  Lib.Files.read_lines_iter "input.txt"
  |> Iter.iter (fun line ->
         let result = Lib.Helpers.find_marker line in
         print_int result;
         print_newline ())
