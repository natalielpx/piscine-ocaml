(* Allowed functions : The Array module *)

(** Write a function named eu_dist which 
  * takes two points and 
  * calculates the Euclidian distance between them *)
let eu_dist arr1 arr2 =
  let n = Array.length arr1 in
  let rec aux acc i =
    if i < n
    then aux (acc +. ((arr1.(i) -. arr2.(i)) ** 2.)) (i + 1)
    else (sqrt acc)
  in
  aux 0. 0

(** Demonstration Program *)
let () =
  print_endline (string_of_float (eu_dist [|4.2|] [|0.42|]));
  print_endline (string_of_float (eu_dist [|4.2; 3.4|] [|0.42; 9.02|]))