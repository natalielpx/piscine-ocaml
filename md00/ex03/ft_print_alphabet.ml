(* Allowed functions : char_of_int, int_of_char, print_char *)

let rec ft_print_next_alphabet i =
  print_char (char_of_int i);
  if i < int_of_char 'z' then
    ft_print_next_alphabet (i + 1)

let ft_print_alphabet () =
  ft_print_next_alphabet (int_of_char 'a');
  print_char '\n'

(* Demonstration Program *)
let () =
  ft_print_alphabet ()