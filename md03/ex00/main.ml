(* Allowed functions : Nothing *)

let () =
  print_endline "Types of Cards:";
  let rec aux = function
  | [] -> ()
  | t :: ts ->
    print_endline ((Color.toString t) ^ " represents " ^ (Color.toStringVerbose t));
    aux ts
  in
  aux Color.all