(* Allowed functions : String module and not the @ operator *)
(* Are print functions allowed? *)

let rec ft_power x y =
  if x < 0 || y < 0 then raise (Invalid_argument "Both x and y must be positive");
  let rec aux acc = function
    | (_, 0)    -> 1
    | (0, _)    -> 0
    | (a, b)    -> aux (acc * a) (a, b - 1)
  in
  aux 1 (x, y)

let get_target index num =
  let rec aux n = 
    if (index mod (ft_power 2 (n - 1))) = 0
    then num - n
    else aux (n - 1)
  in
  aux num

let gray_code prev index number =
  let target = get_target (index + 1) number in
  let rec aux next i =
    if i = number then next
    else
      let c =
        if i = target
        then if prev.[i] = '0' then '1' else '0'
        else prev.[i]
      in
      aux (next ^ String.make 1 c) (i + 1)
  in
  aux "" 0

let gray number =
  let index = (ft_power 2 number) - 1 in
  let rec aux code i =
    print_string code;
    if i >= index then ()
    else begin
      print_char ' ';
      aux (gray_code code i number) (i + 1)
    end
  in
  aux (String.make number '0') 0;
  print_newline ()

(* Demonstration Program *)

let () =
  gray 1;
  gray 2;
  gray 3;
  gray 4