(** Allowed functions : print_endline *)
let ft_test_sign x =
  let sign =
    if x < 0 then "negative"
    else          "positive"
  in
  print_endline sign

(** Demonstration program *)
let () =
  ft_test_sign (-42);
  ft_test_sign 0;
  ft_test_sign (-0);
  ft_test_sign 1;
  ft_test_sign 42
