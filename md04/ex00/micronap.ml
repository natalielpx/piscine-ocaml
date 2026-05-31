(* Allowed functions : The Sys and Array modules *)

(* Function provided by 42 *)
let my_sleep () = Unix.sleep 1

let sleep number =
  for _ = 1 to number do
    my_sleep ()
  done

let () =

  if (Array.length Sys.argv <> 2) then failwith "Incorrect number of arguments";

  let number =
    try int_of_string Sys.argv.(1)
    with Failure _ -> failwith "Please input a number"
  in

  if number <= 0 then failwith "Please input a natural number";
  
  sleep number

(* Compilation Command: ocamlopt unix.cmxa micronap.ml *)