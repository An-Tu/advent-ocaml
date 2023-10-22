let () =
  let a, b, c =
    Lib.Files.read_lines_iter "./input.txt"
    |> Iter.map (Option.map int_of_string)
    |> Lib.Helpers.batching_iter (fun it next ->
           let acc = ref 0 in
           it (fun x ->
               match x with
               | None ->
                   next !acc;
                   acc := 0
               | Some el -> acc := !acc + el);
           if !acc > 0 then next !acc)
    |> Iter.fold
         (fun (a, b, c) el ->
           match (a, b, c) with
           | a, _, _ when el > a -> (el, a, b)
           | _, b, _ when el > b -> (a, el, b)
           | _, _, c when el > c -> (a, b, el)
           | _ -> (a, b, c))
         (0, 0, 0)
  in

  Printf.printf "Groups: %d, %d, %d.\nSum: %d.\n" a b c (a + b + c)
