let () =
  let a, b, c =
    Lib.Files.read_lines_seq "./input.txt"
    |> Seq.map (Option.map int_of_string)
    |> Lib.Helpers.batching_seq (fun seq ->
           let rec iter acc seq =
             match seq () with
             | Seq.Nil -> (acc, seq)
             | Cons (x, xs) -> (
                 match x with None -> (acc, xs) | Some v -> iter (acc + v) xs)
           in
           iter 0 seq)
    |> Seq.fold_left
         (fun (a, b, c) el ->
           match (a, b, c) with
           | a, _, _ when el > a -> (el, a, b)
           | _, b, _ when el > b -> (a, el, b)
           | _, _, c when el > c -> (a, b, el)
           | _ -> (a, b, c))
         (0, 0, 0)
  in

  Printf.printf "Groups: %d, %d, %d.\nSum: %d.\n" a b c (a + b + c)
