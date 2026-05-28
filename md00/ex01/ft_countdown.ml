(** Allowed functions : print_int, print_char *)
let rec ft_countdown x =
  print_int (if x > 0 then x else 0);
  print_char '\n';
  if x > 0 then ft_countdown (x - 1)

(** Demonstration program *)
let () =
  ft_countdown (-42);
  print_newline ();
  ft_countdown (0);
  print_newline ();
  ft_countdown (3);
  print_newline ();
  ft_countdown (42);
  print_newline ()