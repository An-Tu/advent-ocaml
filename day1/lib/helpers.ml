let rec batching_seq fn seq () =
  let open Seq in
  match seq () with
  | Nil -> Nil
  | Cons (x, xs) ->
      let res, next_seq = fn (Seq.cons x xs) in
      Cons (res, batching_seq fn next_seq)

let[@inline] batching_iter fn seq k = fn seq k
