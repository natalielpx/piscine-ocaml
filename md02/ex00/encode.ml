(* Allowed functions : None *)

(** 
  * encodes a list of elements
  * to a list of tuples containing
  * the element and the number of times it repeats
*)
let encode elems =
  let rec aux list count char elems =
    match elems with
    | [] ->
      list @ [(count, char)]
    | x :: tail ->
      if x = char then
        aux list (count + 1) char tail
      else
        aux (list @ [(count, char)]) 1 x tail
  in
  match elems with
    | [] -> []
    | elem :: tail -> aux [] 1 elem tail

(* Demonstration Program *)

let rec print_char_list list =
  match list with
  | [] -> print_newline ()
  | x :: tail ->
      print_char x;
      print_char_list tail

let print_pair (a, b) =
  print_int a;
  print_char ' ';
  print_int b;
  print_newline ()

let rec print_int_char_tuple_list list =
  match list with
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