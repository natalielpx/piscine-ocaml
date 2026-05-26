(* Allowed functions : None *)

let ltos list =
  let rec aux str lst =
    match lst with
    | [] -> str
    | l :: tail -> aux (str ^ l) tail
  in
  aux "" list

let itoa_pos number =
  let hex = [|"0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"|] in
  let rec aux res num =
    if num < 10 then  hex.(num) ^ res
    else              aux (hex.(num mod 10) ^ res) (num / 10)
  in
  aux "" number
  
let get_next_sequence prev =
  let rec aux next count el tail =
    match tail with
    | [] -> (next @ [(itoa_pos count); el])
    | e :: t ->
      if e = el then
        aux next (count + 1) e t
      else
        aux (next @ [(itoa_pos count); el]) 1 e t
  in
  match prev with
  | [] -> []
  | elem :: rest -> aux [] 1 elem rest

let sequence number =
  if number < 1 then raise (Invalid_argument "number must be larger than 1");
  let rec aux res n =
    if n = number then  ltos res
    else                aux (get_next_sequence res) (n + 1)
  in
  aux ["1"] 1

(* Demonstration Program *)

let () =
  print_string (sequence 1);
  print_newline ();
  print_string (sequence 2);
  print_newline ();
  print_string (sequence 3);
  print_newline ();
  print_string (sequence 4);
  print_newline ();
  print_string (sequence 5);
  print_newline ();
  print_string (sequence 6);
  print_newline ();
  print_string (sequence 7);
  print_newline ();
  print_string (sequence 8);
  print_newline ()