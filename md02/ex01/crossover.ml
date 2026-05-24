(* Allowed functions : None *)

let found char list =
  let rec aux c l =
    match l with
    | [] -> false
    | el :: tail ->
      if c = el then
        true
      else
        aux c tail
  in
  aux char list

let crossover list1 list2 =
  let rec aux1 res l1 = 
    match l1 with
    | [] -> res
    | c1 :: tail1 ->
      if found c1 res || not (found c1 list2) then
        aux1 res tail1
      else
        aux1 (c1 :: res) tail1
  in
  aux1 [] list1


(* Demonstration Program *)

let rec print_char_list list =
  match list with
  | [] -> print_newline ()
  | x :: tail ->
      print_char x;
      print_char_list tail

let () =
  let l1 = ['a'; 'a'; 'a'; 'b'] in
  let l2 = ['a'; 'a'; 'a'; 'b'] in
  print_char_list (crossover l1 l2);
  let l1 = ['a'; 'c'; 'v'] in
  let l2 = ['a'; 'a'; 'a'; 'b'; 'c'] in
  print_char_list (crossover l1 l2);
  let l1 = ['a'; 'a'; 'a'; 'b'; 'b'; 'c'; 'z'; 'z'; 'z'; 'z'] in
  let l2 = ['a'; 'a'; 'a'; 'b'; 'c'] in
  print_char_list (crossover l1 l2)