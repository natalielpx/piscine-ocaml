(** Allowed functions : Nothing *)
let rec ft_power x y =
  if x < 0 || y < 0 || (x = 0 && y = 0) then
    -1
  else if x = 0 then
    0
  else if y = 0 then
    1
  else
    x * ft_power x (y - 1)

(** Demonstration program *)
let () =
  print_int (ft_power (-1) 42);
  print_newline ();
  print_int (ft_power 42 (-1));
  print_newline ();
  print_int (ft_power 0 0);
  print_newline ();
  print_int (ft_power 0 2);
  print_newline ();
  print_int (ft_power 2 0);
  print_newline ();
  print_int (ft_power 2 5);
  print_newline ()