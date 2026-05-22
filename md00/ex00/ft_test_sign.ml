(** Allowed functions : print_endline *)
let ft_test_sign x =
  let sign =
    if x < 0 then "negative"
    else          "positive"
  in
  print_endline sign