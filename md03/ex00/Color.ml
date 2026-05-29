(* Allowed functions : Nothing *)

type t = Spade | Heart | Diamond | Club

(** The list of all values of type t
  * val all : t list *)
let all = [Spade; Heart; Diamond; Club]

(** "S", "H", "D" or "C"
  * val toString : t -> string *)
let toString = function
  | Spade   -> "S"
  | Heart   -> "H"
  | Diamond -> "D"
  | Club    -> "C"

(** "Spade", "Heart", "Diamond", "Club"
  * val toStringVerbose : t -> string *)
let toStringVerbose = function
  | Spade   -> "Spade"
  | Heart   -> "Heart"
  | Diamond -> "Diamond"
  | Club    -> "Club"