module Crate : sig
  type t

  val make : string -> t
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end = struct
  type t = string

  let make c = c
  let to_string t = t
  let pp = Format.pp_print_string
end

module Instruction : sig
  type t

  val make : quantity:int -> src:int -> dst:int -> t
  val get_quantity : t -> int
  val get_src : t -> int
  val get_dst : t -> int
  val pp : Format.formatter -> t -> unit
end = struct
  type t = { quantity : int; src : int; dst : int }

  let make ~quantity ~src ~dst = { quantity; src; dst }
  let get_quantity t = t.quantity
  let get_src t = t.src
  let get_dst t = t.dst

  let pp fmt { quantity; src; dst } =
    Format.fprintf fmt "Instruction { quantity: %d, src: %d, dst: %d }\n"
      quantity src dst
end

module Instructions = struct
  type t = Instruction.t list

  let make ins =
    ins
    |> List.map (fun (quantity, src, dst) ->
           Instruction.make ~quantity ~src:(src - 1) ~dst:(dst - 1))
end

module Pile : sig
  type t = Crate.t list

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

module Piles : sig
  type t = Pile.t list

  val make : string option list list -> t
  val apply : Instruction.t -> t -> t
  val pp : Format.formatter -> t -> unit
end = struct
  type t = Pile.t list

  let make crates = crates |> Lib.Helpers.transpose |> List.map Pile.make

  let apply instruction t =
    let quantity = Instruction.get_quantity instruction in
    let src = Instruction.get_src instruction in
    let dst = Instruction.get_dst instruction in
    if quantity = 0 then t
    else
      let tmp = ref [] in
      t
      |> List.mapi (fun idx pile ->
             if idx = src then (
               let count = ref quantity in
               let move, data =
                 List.partition
                   (fun _ ->
                     let should_take = !count > 0 in
                     count := !count - 1;
                     should_take)
                   pile
               in
               tmp := move;
               data)
             else pile)
      |> List.mapi (fun idx pile ->
             if idx = dst then
               match !tmp with
               | [] -> pile
               | [ el ] -> el :: pile
               | move -> List.append (move |> List.rev) pile
             else pile)

  let pp tmt t =
    t
    |> List.iteri (fun i pile ->
           Format.fprintf tmt "Pile %d: %a" i Pile.pp pile)
end

let parse_line (crates, instructions) line =
  match Angstrom.parse_string ~consume:Prefix Lib.Parser.parse_line line with
  | Ok v -> (
      match v with
      | Crate_line cs -> (cs :: crates, instructions)
      | Instruction ins -> (crates, ins :: instructions)
      | Unknown -> (crates, instructions))
  | Error msg -> failwith msg

let print_piles piles = Format.printf "%a" Piles.pp piles

let print_instruction instruction =
  Format.printf "%a" Instruction.pp instruction

let _ =
  let lines = Lib.Files.read_lines_iter "input.txt" in
  let crates, instructions = Iter.fold parse_line ([], []) lines in
  let piles = crates |> List.rev |> Piles.make in
  let instructions = instructions |> List.rev |> Instructions.make in
  piles |> print_piles;
  Format.print_newline ();
  let piles =
    instructions
    |> List.fold_left
         (fun acc instruction ->
           print_instruction instruction;
           let piles = Piles.apply instruction acc in
           piles |> print_piles;
           Format.print_newline ();
           piles)
         piles
  in
  let res =
    piles
    |> List.fold_left
         (fun acc pile ->
           match pile with [] -> acc | x :: _ -> acc ^ Crate.to_string x)
         ""
  in
  Format.printf "answer = %s\n" res
