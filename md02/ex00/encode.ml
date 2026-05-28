(* Allowed functions : None *)

(** Reverses a list *)
let rev = function
  | []            -> []
  | elem :: []    -> [elem]
  | elem :: elems ->
    let rec aux acc = function
      | []          -> acc
      | el :: tail  -> aux (el :: acc) tail
    in
    aux [] elems

(** 
  * Encodes a list of elements
  * to a list of tuples containing
  * the element and the number of times it repeats
*)
let encode = function
  | [] -> []
  | elem :: elems ->
    let rec aux acc count char = function
      | []                     ->  rev ((count, char) :: acc)
      | x :: xs when x = char  ->  aux acc (count + 1) char  xs
      | x :: xs                ->  aux ((count, char) :: acc) 1 x xs
    in
    aux [] 1 elem elems

(* Demonstration Program *)

let rec print_char_list = function
  | [] -> print_newline ()
  | c :: cs ->
    print_char c;
    print_char_list cs

let rec print_int_char_tuple_list = function
  | [] -> print_newline ()
  | (a, b) :: tail ->
    print_int a;
    print_char b;
    print_int_char_tuple_list tail

let () =
  let chars = ['a'; 'a'; 'a'; 'b'] in
  print_char_list chars;
  print_int_char_tuple_list (encode chars);
  let chars = ['a'; 'a'; 'a'] in
  print_char_list chars;
  print_int_char_tuple_list (encode chars);
  let chars = ['a'; 'a'; 'a'; 'b'; 'b'; 'c'; 'z'; 'z'; 'z'; 'z'] in
  print_char_list chars;
  print_int_char_tuple_list (encode chars)