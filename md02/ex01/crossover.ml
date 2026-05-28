(* Allowed functions : None *)

let found char lst =
  let rec aux = function
    | []                     -> false
    | c :: cs when c = char  -> true
    | c :: cs                -> aux cs
  in
  aux lst

let crossover lst1 lst2 =
  let rec aux res = function
    | []                              -> res
    | c :: cs when found c res        -> aux res cs         (* element is already a duplicate*)
    | c :: cs when not (found c lst2) -> aux res cs         (* element is not in second list*)
    | c :: cs                         -> aux (c :: res) cs
  in
  aux [] lst1


(* Demonstration Program *)

let rec print_char_list = function
  | [] -> print_newline ()
  | c :: cs ->
      print_char c;
      print_char_list cs

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