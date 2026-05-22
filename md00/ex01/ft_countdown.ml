(** Allowed functions : print_int, print_char *)
let rec ft_countdown x =
  let n =
    if x > 0 then x
    else          0
  in
  print_int n;
  print_char '\n';
  if x > 0 then ft_countdown (x - 1)