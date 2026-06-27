(* Allowed functions : Stdlib.fst and Stdlb.snd *)

module type PAIR = sig val pair : (int * int) end                 (* Given by the subject *)
module type VAL = sig val x : int end                             (* Given by the subject *)

(* TO DO *)
module MakeFst(P: PAIR) : VAL = struct let x = Stdlib.fst P.pair end
module MakeSnd(P: PAIR) : VAL = struct let x = Stdlib.snd P.pair end
(* reference: https://ocaml.org/docs/functors *)

module Pair : PAIR = struct let pair = ( 21, 42 ) end             (* Given by the subject *)
module Fst : VAL = MakeFst (Pair)                                 (* Given by the subject *)
module Snd : VAL = MakeSnd (Pair)                                 (* Given by the subject *)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x     (* Given by the subject *)