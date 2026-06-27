(* Forbidden functions : None *)

let one_nn stats path =
  
  (* Obtain same length radars *)
  let len = List.length stats in
  let lst = examples_of_file "./ionosphere.train.csv" in
  let rec aux acc = function
    | [] -> acc
    | (flts, cls) as hd :: tl  where (Array.length flts) = len -> aux (hd :: acc) tl
    | hd :: tl -> aux acc tl
    
  in
  aux [] stats

let () =
