(* Allowed functions : None *)

let sum f1 f2 =
  f1 +. f2

(** Demonstration Program *)
let () =
  print_endline (string_of_float (sum 4.2 0.42));
  print_endline (string_of_float (sum 4.2382937 0.42));
  print_endline (string_of_float (sum 4.2 0.42238974));
  print_endline (string_of_float (sum 4.2 (-129837.42)))