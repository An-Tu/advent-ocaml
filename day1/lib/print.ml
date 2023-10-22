let pp_option_str fmt o =
  Format.(
    pp_print_option
      ~none:(fun fmt () -> pp_print_string fmt "None\n")
      (fun fmt str -> fprintf fmt "Some(%s)\n" str)
      fmt o)

let pp_option_int fmt o =
  Format.(
    pp_print_option
      ~none:(fun fmt () -> pp_print_string fmt "None\n")
      (fun fmt num -> fprintf fmt "Some(%d)\n" num)
      fmt o)
