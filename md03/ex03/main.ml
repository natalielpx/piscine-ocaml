let print_deck lst = 
  let rec aux = function
    | [] -> ()
    | card :: cards ->
      print_string ((Deck.Card.toString card) ^ " ");
      aux cards
    in
    aux lst

let () =

  print_string "Random Deck 1:";
  print_deck (Deck.newDeck ());
  print_newline ();
  print_string "Random Deck 2:";
  print_deck (Deck.newDeck ());
  print_newline ();
  print_string "Random Deck 3:";
  print_deck (Deck.newDeck ());
  print_newline ();
  print_string "Random Deck 4:";
  print_deck (Deck.newDeck ());
  print_newline ();

  print_newline ();
  print_endline "New Deck:";
  let deck = Deck.newDeck () in
  print_deck deck;
  print_newline ();
  print_endline "Draw a card from deck...";
  let ((value, color) as card, remaining) = Deck.drawCard deck in
  print_endline ("Card drawn: " ^ (Deck.Card.toString card) ^ " of value " ^ (Deck.Card.Value.toStringVerbose value) ^ " and color " ^ (Deck.Card.Color.toStringVerbose color));
  print_string "Cards remaining: ";
  print_deck remaining



