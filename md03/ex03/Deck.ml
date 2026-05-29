(* Allowed functions : Allowed functions and modules from the previous exercices, plus raise and the Random module *)
(* invalid_arg, Printf.sprintf, List module, raise, Random module *)

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

module Card = struct
  module Value = Value
  module Color = Color
  type t = (Value.t * Color.t)
  let newCard value color = (value, color)
  let allSpades = List.map (fun v -> newCard v Color.Spade) Value.all
  let allHearts = List.map (fun v -> newCard v Color.Heart) Value.all
  let allDiamonds = List.map (fun v -> newCard v Color.Diamond) Value.all
  let allClubs = List.map (fun v -> newCard v Color.Club) Value.all
  let all = allSpades @ allHearts @ allDiamonds @ allClubs
  let getValue (value, _) = value
  let getColor (_, color) = color
  let toString (value, color) = (Value.toString value) ^ (Color.toString color)
  let toStringVerbose (value, color) = "Card(" ^ (Value.toStringVerbose value) ^ ", " ^ (Color.toStringVerbose color) ^ ")"
  let compare (v1, _) (v2, _) = (Value.toInt v1) - (Value.toInt v2)
  let max c1 c2 = if compare c1 c2 >= 0 then c1 else c2
  let min c1 c2 = if compare c1 c2 <= 0 then c1 else c2
  let best = function
    | []            -> invalid_arg "empty list"
    | card :: cards -> List.fold_left max card cards
  let isOf (_, color) c = (color = c)
  let isSpade card = isOf card Color.Spade
  let isHeart card = isOf card Color.Heart
  let isDiamond card = isOf card Color.Diamond
  let isClub card = isOf card Color.Club
end

(* Abstract type t that represents a deck. *)
type t = Card.t list

(** Write a function newDeck that 
  * takes no argument and 
  * returns a deck of the 52 cards (i.e. the type t) in random order.
  * Upon two different calls, 
  * the order of the deck will be different. *)
let newDeck () =
  let compare (x, _) (y, _) = x - y in
  let random = List.map (fun c -> (Random.int 1000000, c)) Card.all in
  List.map (fun (_, c) -> c) (List.sort compare random)


(** Write a function toStringList that 
  * takes a deck as a parameter and 
  * returns a list of the string representations of each card. *)
let toStringList deck =
  let rec aux acc = function
  | []      -> acc
  | c :: cs -> aux ((Card.toString c) :: acc ) cs
  in
  aux [] (List.rev deck)

(** Write a function toStringListVerbose that 
  * takes a deck as a parameter and 
  * returns a list of the verbose string representations of each card. *)
let toStringListVerbose deck =
  let rec aux acc = function
    | []      -> acc
    | c :: cs -> aux ((Card.toStringVerbose c) :: acc ) cs
  in
  aux [] (List.rev deck)

(** Write a function drawCard that 
  * takes a deck as a parameter and 
  * returns a couple composed of 
  * the first card of the deck and 
  * the rest of the deck.
  * If the deck is empty, 
  * raise the exception Failure with a relevant error message. *)
let drawCard = function
  | card :: rest  -> (card, rest)
  | []            -> raise (Failure "Deck is empty")