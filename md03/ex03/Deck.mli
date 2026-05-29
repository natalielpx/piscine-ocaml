(* Embeds the Card module. *)
module Color :
sig
  type t = Spade | Heart | Diamond | Club
  val all : t list
  val toString : t -> string
  val toStringVerbose : t -> string
end

module Value :
sig
  type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
  val all : t list
  val toInt : t -> int
  val toString : t -> string
  val toStringVerbose : t -> string
  val next : t -> t
  val previous : t -> t
end

module Card :
sig
  module Value = Value
  module Color = Color
  type t = (Value.t * Color.t)
  val newCard : Value.t -> Color.t -> t
  val allSpades : t list
  val allHearts : t list
  val allDiamonds : t list
  val allClubs : t list
  val all : t list
  val getValue : t -> Value.t
  val getColor : t -> Color.t
  val toString : t -> string
  val toStringVerbose : t -> string
  val compare : t -> t -> int
  val max : t -> t -> t
  val min : t -> t -> t
  val best : t list -> t
  val isOf : t -> Color.t -> bool
  val isSpade : t -> bool
  val isHeart : t -> bool
  val isDiamond : t -> bool
  val isClub : t -> bool
end

(* Exposes an abstract type t that represents a deck. *)
type t = Card.t list

(** Exposes a function newDeck that 
  * takes no argument and 
  * returns a deck of the 52 cards (i.e. the type t) in random order.
  * Upon two different calls, 
  * the order of the deck will be different. *)
val newDeck : unit -> t

(** Exposes a function toStringList that 
  * takes a deck as a parameter and 
  * returns a list of the string representations of each card. *)
val toStringList : t -> string list

(** Exposes a function toStringListVerbose that 
  * takes a deck as a parameter and 
  * returns a list of the verbose string representations of each card. *)
val toStringListVerbose : t -> string list

(** Exposes a function drawCard that 
  * takes a deck as a parameter and 
  * returns a couple composed of 
  * the first card of the deck and 
  * the rest of the deck.
  * If the deck is empty, 
  * raise the exception Failure with a relevant error message. *)
val drawCard : t -> (Card.t * Card.t list)