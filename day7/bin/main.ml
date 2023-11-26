let read_file file = In_channel.with_open_bin file In_channel.input_all

let () =
  let file = read_file "input.txt" in
  match Angstrom.parse_string ~consume:Prefix Lib.Parser.parse_all file with
  | Ok v ->
      v
      |> List.iter (fun v ->
             v |> Lib.Parser.to_string |> print_string;
             print_newline ())
  | Error msg -> failwith msg
