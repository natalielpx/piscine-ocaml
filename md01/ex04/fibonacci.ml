(* Allowed functions : None *)
let fibonacci number =
  if      number < 0 then -1
  else if number = 0 then 0
  else if number = 1 then 1
  else
    let rec aux acc1 acc2 n =
      if n = number
      then acc1 + acc2
      else aux acc2 (acc1 + acc2) (n + 1)
    in
    aux 0 1 2

(** Demonstration Program *)
let () =
  print_int (fibonacci (-42));
  print_newline();
  print_int (fibonacci 0);
  print_newline();
  print_int (fibonacci 1);
  print_newline();
  print_int (fibonacci 6);
  print_newline();
  print_int (fibonacci 80); (* F80 = 2.3416728348468E+16 *)
  print_newline()