let string_to_array_chars s =
  let l = String.length s in
  let res = Array.make l 'a' in

  for i = 0 to l - 1 do
    Array.set res i (String.get s i)
  done;
  res

let string_to_list_chars s =
  let rec iter str i =
    if String.length str = i then [] else String.get str i :: iter str (i + 1)
  in
  iter s 0
