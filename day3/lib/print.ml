let pp_print_array v_pp fmt arr =
  let l = Array.length arr in
  let iter fmt arr =
    arr
    |> Array.iteri (fun idx el ->
           Format.fprintf fmt (if idx = l - 1 then "%a" else "%a, ") v_pp el)
  in
  Format.fprintf fmt "[%a]" iter arr
