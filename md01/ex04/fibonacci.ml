(* Allowed functions : None *)
let fibonacci n =
  let rec aux acc n = 
    if      n < 0 then  -1
    else if n = 0 then  acc
    else if n = 1 then  acc + 1
    else                (aux acc (n - 2)) + (aux acc (n - 1));
  in
  aux 0 n

(** Demonstration Program *)
let () =
  print_int (fibonacci (-42));
  print_newline();
  print_int (fibonacci 1);
  print_newline();
  print_int (fibonacci 3);
  print_newline();
  print_int (fibonacci 6);
  print_newline();
  print_int (fibonacci 20);
  print_newline()