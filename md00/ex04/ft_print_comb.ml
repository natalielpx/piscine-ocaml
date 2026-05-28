(* Allowed functions : print_int, print_string *)

let ft_print_num h t o =
  print_int h;
  print_int t;
  print_int o;
  if (h < 7 || t < 8 || o < 9) then print_string ", "

let rec ft_ones h t o =
  ft_print_num h t o;
  if o < 9 then ft_ones h t (o + 1)

let rec ft_tens h t =
  ft_ones h t (t + 1);
  if t < 8 then ft_tens h (t + 1)

let rec ft_hundreds h =
  ft_tens h (h + 1);
  if h < 7 then ft_hundreds (h + 1)

let ft_print_comb () =
  ft_hundreds 0;
  print_string "\n"

(* Demonstration Program *)
let () =
  ft_print_comb ()