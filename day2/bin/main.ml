module Outcome = struct
  type t = Win | Draw | Loss [@@deriving show]

  let inherent_points = function Win -> 6 | Draw -> 3 | Loss -> 0

  let from_char = function
    | 'X' -> Ok Loss
    | 'Y' -> Ok Draw
    | 'Z' -> Ok Win
    | c -> Error ("not valid outcome: " ^ Char.escaped c)
end

module Move = struct
  type t = Rock | Paper | Scissors [@@deriving show]

  let from_char = function
    | 'A' -> Ok Rock
    | 'B' -> Ok Paper
    | 'C' -> Ok Scissors
    | c -> Error ("not valid move: " ^ Char.escaped c)

  let inherent_points = function Rock -> 1 | Paper -> 2 | Scissors -> 3

  let winning_move = function
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock

  let losing_move = function
    | Rock -> Scissors
    | Scissors -> Paper
    | Paper -> Rock

  let drawing_move t = t

  let outcome ~ours ~theirs =
    if winning_move ours = theirs then Outcome.Loss
    else if losing_move ours = theirs then Outcome.Win
    else Outcome.Draw

  let matching_move outcome theirs =
    match outcome with
    | Outcome.Win -> winning_move theirs
    | Outcome.Draw -> drawing_move theirs
    | Outcome.Loss -> losing_move theirs
end

module Round = struct
  type t = { theirs : Move.t; ours : Move.t } [@@deriving show]

  let from_str str =
    try
      let theirs = String.get str 0 |> Move.from_char |> Result.get_ok in
      let outcome = String.get str 2 |> Outcome.from_char |> Result.get_ok in
      Ok { theirs; ours = Move.matching_move outcome theirs }
    with Invalid_argument _ ->
      Error ("expected <theirs>SP<outcome>, got: " ^ str)

  let outcome t = Move.outcome ~ours:t.ours ~theirs:t.theirs

  let our_score t =
    Move.inherent_points t.ours + Outcome.inherent_points (outcome t)
end

let sum =
  Lib.Files.read_lines_iter "./input.txt"
  |> Iter.filter_map (Option.map Round.from_str)
  |> Iter.map (fun round ->
         let r = round |> Result.get_ok in
         Format.printf "%a@." Round.pp r;
         Round.our_score r)
  |> Iter.sum

let () = Printf.printf "total score: %d\n" sum
