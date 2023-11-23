let[@inline] batching_iter fn seq k = fn seq k

module SS = Set.Make (Char)

let tuple_len = 4

let make_tuple_4_buffer () =
  let acc = ref (None, None, None, None) in
  fun x ->
    (match !acc with
    | None, None, None, None -> acc := (Some x, None, None, None)
    | Some a, None, None, None -> acc := (Some a, Some x, None, None)
    | Some a, Some b, None, None -> acc := (Some a, Some b, Some x, None)
    | Some a, Some b, Some c, None -> acc := (Some a, Some b, Some c, Some x)
    | Some _, Some b, Some c, Some d -> acc := (Some b, Some c, Some d, Some x)
    | _ -> failwith "unreachable case");
    !acc

let find_marker str =
  str |> Iter.of_str
  |> batching_iter (fun it next ->
         let tuple_buffer = make_tuple_4_buffer () in
         it (fun x ->
             match tuple_buffer x with
             | Some a, Some b, Some c, Some d -> next (a, b, c, d)
             | _ -> ()))
  |> Iter.findi (fun idx (a, b, c, d) ->
         let arr = Array.make 256 false in
         if
           [ a; b; c; d ]
           |> List.exists (fun el ->
                  let code = Char.code el in
                  let exists = Array.get arr code in
                  Array.set arr code true;
                  exists)
         then None
         else Some idx)
  |> Option.map (fun position -> position + tuple_len)
  |> Option.value ~default:0
