(* Allowed functions : invalid_arg, Printf.sprintf and the List module *)

module Color = struct

  type t = Spade | Heart | Diamond | Club

  let all = [Spade; Heart; Diamond; Club]

  let toString        = function | Spade -> "S"     | Heart -> "H"      | Diamond -> "D"        | Club -> "C"
  let toStringVerbose = function | Spade -> "Spade" | Heart -> "Heart"  | Diamond -> "Diamond"  | Club -> "Club"
end

module Value = struct

  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
  
  let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]
  
  let toInt           = function | T2 -> 1   | T3 -> 2   | T4 -> 3   | T5 -> 4   | T6 -> 5   | T7 -> 6   | T8 -> 7   | T9 -> 8   | T10 -> 9    | Jack -> 10     | Queen -> 11      | King -> 12     | As -> 13
  let toString        = function | T2 -> "2" | T3 -> "3" | T4 -> "4" | T5 -> "5" | T6 -> "6" | T7 -> "7" | T8 -> "8" | T9 -> "9" | T10 -> "10" | Jack -> "J"    | Queen -> "Q"     | King -> "K"    | As -> "A"
  let toStringVerbose = function | T2 -> "2" | T3 -> "3" | T4 -> "4" | T5 -> "5" | T6 -> "6" | T7 -> "7" | T8 -> "8" | T9 -> "9" | T10 -> "10" | Jack -> "Jack" | Queen -> "Queen" | King -> "King" | As -> "As"
  
  let previous = function | T2 -> invalid_arg "No prev value"
                          | T3 -> T2 | T4 -> T3 | T5 -> T4 | T6 -> T5 | T7 -> T6 | T8 -> T7 | T9 -> T8 | T10 -> T9 | Jack -> T10 | Queen -> Jack | King -> Queen | As -> King
  let next     = function | T2 -> T3 | T3 -> T4 | T4 -> T5 | T5 -> T6 | T6 -> T7 | T7 -> T8 | T8 -> T9 | T9 -> T10 | T10 -> Jack | Jack -> Queen | Queen -> King | King -> As
                          | As -> invalid_arg "No next value"
end

(* type t *)
type t = (Value.t * Color.t)

(* val newCard : Value.t -> Color.t -> t *)
let newCard value color = (value, color)

(* val allSpades : t list *)
let allSpades =
  let rec aux acc = function
    | [] -> List.rev acc
    | v :: vs -> aux ((v, Color.Spade) :: acc) vs
  in
  aux [] Value.all

(* val allHearts : t list *)
let allHearts =
  let rec aux acc = function
    | [] -> List.rev acc
    | v :: vs -> aux ((v, Color.Heart) :: acc) vs
  in
  aux [] Value.all

(* val allDiamonds : t list *)
let allDiamonds =
  let rec aux acc = function
    | [] -> List.rev acc
    | v :: vs -> aux ((v, Color.Diamond) :: acc) vs
  in
  aux [] Value.all

(* val allClubs : t list *)
let allClubs =
  let rec aux acc = function
    | [] -> List.rev acc
    | v :: vs -> aux ((v, Color.Club) :: acc) vs
  in
  aux [] Value.all

(* val all : t list *)
let all = allSpades @ allHearts @ allDiamonds @ allClubs

(* val getValue : t -> Value.t *)
let getValue (value, _) = value

(* val getColor : t -> Color.t *)
let getColor (_, color) = color

(* val toString : t -> string *)
let toString (value, color) = (Value.toString value) ^ (Color.toString color)

(* val toStringVerbose : t -> string *)
let toStringVerbose (value, color) = "Card(" ^ (Value.toStringVerbose value) ^ ", " ^ (Color.toStringVerbose color) ^ ")"

(* val compare : t -> t -> int *)
let compare (v1, _) (v2, _) = (Value.toInt v1) - (Value.toInt v2)

(* val max : t -> t -> t *)
let max c1 c2 = if compare c1 c2 >= 0 then c1 else c2

(* val min : t -> t -> t *)
let min c1 c2 = if compare c1 c2 <= 0 then c1 else c2

(* val best : t list -> t *)
let best = function
  | []      -> invalid_arg "empty list"
  | card :: cards ->
    let rec aux res = function
      | []      -> res
      | c :: cs -> aux (max res c) cs
    in
    aux card cards

(* val isOf : t -> Color.t -> bool *)
let isOf (_, color) c = (color = c)

(* val isSpade : t -> bool *)
let isSpade card = isOf card Color.Spade

(* val isHeart : t -> bool *)
let isHeart card = isOf card Color.Heart


(* val isDiamond : t -> bool *)
let isDiamond card = isOf card Color.Diamond


(* val isClub : t -> bool *)
let isClub card = isOf card Color.Club
