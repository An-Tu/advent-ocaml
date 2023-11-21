module Crate : sig
  type t

  val make : string -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = string

  let make c = c
  let pp = Format.pp_print_string
end

module Instruction : sig
  type t

  val make : quantity:int -> src:int -> dst:int -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = { quantity : int; src : int; dst : int }

  let make ~quantity ~src ~dst = { quantity; src; dst }

  let pp fmt { quantity; src; dst } =
    Format.fprintf fmt "Instruction { quantity: %d, src: %d, dst: %d }\n"
      quantity src dst
end

module Pile : sig
  type t

  val make : string option list -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = Crate.t list

  let make = Crate.make |> Option.map |> List.filter_map

  let pp fmt t =
    let open Format in
    fprintf fmt "[%a]\n"
      (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") Crate.pp)
      t
end

let rec transpose list =
  match list with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss -> List.((x :: map hd xss) :: transpose (xs :: map tl xss))

let parse_line (crates, instructions) line =
  match Angstrom.parse_string ~consume:Prefix Lib.Parser.parse_line line with
  | Ok v -> (
      match v with
      | Crate_line cs -> (cs :: crates, instructions)
      | Instruction ins -> (crates, ins :: instructions)
      | Unknown -> (crates, instructions))
  | Error msg -> failwith msg

let print_piles piles =
  piles |> List.iteri (fun i pile -> Format.printf "Pile %d: %a" i Pile.pp pile)

let print_instruction instruction =
  Format.printf "%a" Instruction.pp instruction

let _ =
  let lines = Lib.Files.read_lines_iter "input.txt" in
  let crates, instructions = Iter.fold parse_line ([], []) lines in
  let piles = crates |> List.rev |> transpose |> List.map Pile.make in
  let instructions =
    instructions |> List.rev
    |> List.map (fun (quantity, src, dst) ->
           Instruction.make ~quantity ~src:(src - 1) ~dst:(dst - 1))
  in
  piles |> print_piles;
  Format.printf "\n";
  instructions |> List.iter print_instruction
