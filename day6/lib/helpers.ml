let[@inline] batching_iter fn seq k = fn seq k

module SS = Set.Make (Char)

let tuple_len = 4

let find_marker str =
  str |> Iter.of_str
  |> batching_iter (fun it next ->
         let acc = ref (None, None, None, None) in
         it (fun x ->
             match !acc with
             | None, None, None, None -> acc := (Some x, None, None, None)
             | Some a, None, None, None -> acc := (Some a, Some x, None, None)
             | Some a, Some b, None, None ->
                 acc := (Some a, Some b, Some x, None)
             | Some a, Some b, Some c, None ->
                 acc := (Some a, Some b, Some c, Some x);
                 next (a, b, c, x)
             | Some _, Some b, Some c, Some d ->
                 acc := (Some b, Some c, Some d, Some x);
                 next (b, c, d, x)
             | _ -> failwith "unreachable case"))
  |> Iter.findi (fun idx (a, b, c, d) ->
         let set = SS.empty in
         let set = SS.add a set in
         let set = SS.add b set in
         let set = SS.add c set in
         let set = SS.add d set in
         if set |> SS.to_seq |> Seq.length = tuple_len then Some idx else None)
  |> Option.map (fun position -> position + tuple_len)
  |> Option.value ~default:0
