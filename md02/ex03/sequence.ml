(* Allowed functions : None *)

let rev lst=
  let rec aux acc = function
    | []        -> acc
    | l :: tail -> aux (l :: acc) tail
  in
  aux [] lst

let ltos lst =
  let rec aux str = function
    | []        -> str
    | l :: tail -> aux (l ^ str) tail
  in
  aux "" (rev lst)

(* Maximum Consecutive Run Count is at most 3
  A run of 4 would require the prior term
  to have had a count of 11
  (two consecutive digits forming 11),
  which itself would have been collapsed.
  By induction,
  starting from 1,
  no run longer than 3 ever appears. *)
let get_next_sequence prev =
  let rec aux next = function
    | []                                    -> rev next
    | a :: b :: c :: t  when a = b && b = c -> aux (a :: "3" :: next) t
    | a :: b :: t       when a = b          -> aux (a :: "2" :: next) t
    | a :: t                                -> aux (a :: "1" :: next) t
  in
  aux [] prev  

let sequence number =
  if number < 1 then raise (Invalid_argument "number must be larger than 1");
  let rec aux res n =
    if n = number
    then ltos res
    else aux (get_next_sequence res) (n + 1)
  in
  aux ["1"] 1

(* Demonstration Program *)

let () =
  print_string (sequence 1);
  print_newline ();
  print_string (sequence 2);
  print_newline ();
  print_string (sequence 3);
  print_newline ();
  print_string (sequence 4);
  print_newline ();
  print_string (sequence 5);
  print_newline ();
  print_string (sequence 6);
  print_newline ();
  print_string (sequence 7);
  print_newline ();
  print_string (sequence 8);
  print_newline ()