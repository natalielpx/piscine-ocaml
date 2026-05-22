(** Allowed functions : Nothing *)
let rec ft_power x y =
  if x < 0 || y < 0 || (x = 0 && y = 0) then
    -1
  else if x = 0 then
    0
  else if y = 0 then
    1
  else
    x * ft_power x (y - 1)
