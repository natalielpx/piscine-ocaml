let print_deck lst = 
  let rec aux = function
    | [] -> ()
    | card :: cards ->
      print_string ((Deck.Card.toString card) ^ " ");
      aux cards
    in
    aux lst

let () =
  
  Random.self_init ();

  print_endline "Random Deck 1: ";
  print_deck (Deck.newDeck ());
  print_newline ();
  print_endline "Random Deck 2: ";
  print_deck (Deck.newDeck ());
  print_newline ();
  print_endline "Random Deck 3: ";
  print_deck (Deck.newDeck ());
  print_newline ();
  print_endline "Random Deck 4: ";
  print_deck (Deck.newDeck ());
  print_newline ();

  print_newline ();

  (* Create a new deck *)
  print_endline "New Deck:";
  let deck = Deck.newDeck () in
  print_deck deck;
  print_newline ();
  print_newline ();

  (* Draw once from new deck *)
  print_endline "Draw a card from deck...";
  let ((value, color) as card, remaining) = Deck.drawCard deck in
  print_endline ("Card drawn: " ^ (Deck.Card.toString card) ^ " of value " ^ (Deck.Card.Value.toStringVerbose value) ^ " and color " ^ (Deck.Card.Color.toStringVerbose color));
  print_endline "Cards remaining: ";
  print_deck remaining;
  print_newline ();
  print_newline ();

  (* Keep drawing n times *)
  let n = 55 in
  let rec aux i d =
    if i < n then 
      let (c, rem) = Deck.drawCard d in
      print_endline ("Card drawn: " ^ (Deck.Card.toString c));
      aux (i + 1) rem
  in
  aux 0 remaining



