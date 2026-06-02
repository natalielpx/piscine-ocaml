(* Forbidden functions : ref *)

(* Reproduces the ref type. *)
type 'a ft_ref = { mutable contents : 'a }

(** Creates a new reference. *)
let return c = { contents = c }

(** Dereferences a reference. *)
let get r = r.contents

(** Assigns a reference’s value. *)
let set r a = r.contents <- a

(** Applies a function to a reference to transform it. *)
let bind r f = r.contents <- (f r)


(** Demonstration program *)
let () =

  let x = return 1 in
  print_int (get x);
  print_char (' ');
  set x 20;
  print_int (get x);
  print_char (' ');
  let ft_incr a = a.contents + 1 in
  bind x ft_incr;
  print_int (get x);
  print_newline ();

  let y = ref 1 in
  print_int !y;
  print_char (' ');
  y := 20;
  print_int !y;
  print_char (' ');
  incr y;
  print_int !y;
  print_newline ()