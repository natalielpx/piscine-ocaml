(* Forbidden functions : None *)

(** Write a function named examples_of_file that
  * takes a file path as its argument and 
  * returns a set of examples read from the input file,
  * formatted as CSV *)
let examples_of_file path =

  (* Open file *)
  let in_channel = 
    try open_in path
    with e -> raise e
  in

  (* Read lines *)
  let lines =
    let rec aux acc =
      match input_line in_channel with
      | line                  -> aux (line :: acc)
      | exception End_of_file -> close_in in_channel; acc
      | exception e           -> close_in_noerr in_channel; raise e
    in
    aux []
  in

  (* Parse lines (list of strings) into lists of lists of strings *)
  let split = 
    let rec aux acc = function
    | []            -> acc
    | head :: tail  -> aux (((String.split_on_char ',' head)) :: acc) tail
    in
    aux [] lines
  in

  (* Transform split (lists of lists of strings) into arrays of (float array * string) tuples *)
  let radars =
    let func lst = 
      let arr = Array.of_list lst in
      let len = Array.length arr in
      (Array.map float_of_string (Array.sub arr 0 (len - 1)), arr.(len - 1))
    in
    Array.map func (Array.of_list split)
  in

  (* Return result *)
  radars

(** Demonstration Program *)
let () =
  let arr = examples_of_file "/Users/natalie/Downloads/attachment/ionosphere.test.csv" in
  let print_radar (radar : float array * string) =
    let (arr, cls) = radar in
    for i = 0 to Array.length arr - 1 do
      print_float arr.(i);
      print_string ","
    done;
    print_endline cls;
  in
  Array.iter print_radar arr
