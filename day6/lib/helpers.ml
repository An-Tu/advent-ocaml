exception Return of int

let sequence_size = 14
let get_char_code_from_bytes i bytes = i |> Bytes.get bytes |> Char.code

module State = struct
  type t = int array

  let make size = Array.make size 0
  let push c t = t.(c) <- t.(c) + 1
  let pop c t = t.(c) <- t.(c) - 1
  let is_unique t = t |> Array.for_all (fun count -> count <= 1)
end

let find_marker str =
  let bytes = str |> Bytes.of_string in
  if Bytes.length bytes < sequence_size then failwith "string too short"
  else
    let state = State.make 256 in
    for i = 0 to sequence_size do
      let b = get_char_code_from_bytes i bytes in
      State.push b state
    done;

    if State.is_unique state then sequence_size
    else
      try
        for i = 0 to Bytes.length bytes do
          let remove_char_code_in_window = get_char_code_from_bytes i bytes in
          let add_char_code_in_window =
            get_char_code_from_bytes (i + sequence_size + 1) bytes
          in
          State.pop remove_char_code_in_window state;
          State.push add_char_code_in_window state;
          if State.is_unique state then raise (Return (i + 1 + sequence_size))
        done;
        raise (Return 0)
      with Return idx -> idx
