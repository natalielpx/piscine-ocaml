(* Allowed functions : Nothing *)

let () =

  print_endline "Values of Cards:";
  let rec aux = function
  | [] -> ()
  | v :: vs ->
    print_endline ((Value.toString v) ^ " represents " ^ (Value.toStringVerbose v));
    aux vs
  in
  aux Value.all;
  
  print_newline ();

  let v = Value.T2 in
  print_endline ("What comes after " ^ (Value.toStringVerbose v) ^ "?");
  print_endline ("Answer: " ^ (Value.toStringVerbose (Value.next v)));
  print_endline ("What comes before " ^ (Value.toStringVerbose v) ^ "?");
  print_endline ("Answer: " ^ (Value.toStringVerbose (Value.previous v)))
