(* Forbidden functions : None *)

let jokes path =

  (* Open file*)
  let in_channel =
    try open_in path
    with Sys_error _ -> failwith (Printf.sprintf "Unable to open file %s" path)
  in

  (* Load contents *)
  let arr =
    let rec aux acc =
      match input_line in_channel with
      | joke                  -> aux (joke :: acc)
      | exception End_of_file -> close_in in_channel; Array.of_list acc
      | exception e           -> close_in_noerr in_channel; raise e
    in
    aux []
  in

  (* Check not empty *)
  let len = Array.length arr in
  if len = 0
  then print_endline ("No jokes available")

  (* Print random joke *)
  else print_endline arr.(Random.int (Array.length arr))

let () =
  Random.self_init ();
  jokes "./jokes.txt";
  jokes "./empty.txt";
  jokes "./forbidden.txt";