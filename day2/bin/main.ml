module Outcome = struct
  type t = Win | Draw | Loss [@@deriving show]

  let inherent_points = function Win -> 6 | Draw -> 3 | Loss -> 0
end

module Move = struct
  type t = Rock | Paper | Scissors [@@deriving show]

  let from_char = function
    | 'A' | 'X' -> Ok Rock
    | 'B' | 'Y' -> Ok Paper
    | 'C' | 'Z' -> Ok Scissors
    | c -> Error ("not valid move: " ^ Char.escaped c)

  let inherent_points = function Rock -> 1 | Paper -> 2 | Scissors -> 3

  let outcome ~ours ~theirs =
    match (ours, theirs) with
    | Rock, Paper | Paper, Scissors | Scissors, Rock -> Outcome.Loss
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> Outcome.Draw
    | Rock, Scissors | Paper, Rock | Scissors, Paper -> Outcome.Win
end

module Round = struct
  type t = { theirs : Move.t; ours : Move.t } [@@deriving show]

  let from_str str =
    try
      let theirs = String.get str 0 in
      let ours = String.get str 2 in
      Ok
        {
          theirs = theirs |> Move.from_char |> Result.get_ok;
          ours = ours |> Move.from_char |> Result.get_ok;
        }
    with Invalid_argument _ -> Error ("expected <theirs>SP<ours>, got: " ^ str)

  let outcome t = Move.outcome ~ours:t.ours ~theirs:t.theirs

  let our_score t =
    Move.inherent_points t.ours + Outcome.inherent_points (outcome t)
end

let sum =
  Lib.Files.read_lines_iter "./input.txt"
  |> Iter.filter_map (Option.map Round.from_str)
  |> Iter.map (fun round -> round |> Result.get_ok |> Round.our_score)
  |> Iter.sum

let () = Printf.printf "total score: %d\n" sum
