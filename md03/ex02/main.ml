let print_cards lst = 
  let rec aux = function
    | [] -> ()
    | card :: cards ->
      print_endline ((Card.toString card) ^ " " ^ (Card.toStringVerbose card));
      aux cards
    in
    aux lst

let () =

  print_endline "All Cards:";
  print_cards Card.all;
  print_newline ();
  print_endline "All Spades:";
  print_cards Card.allSpades;
  print_newline ();
  print_endline "All Hearts:";
  print_cards Card.allHearts;
  print_newline ();
  print_endline "All Diamonds:";
  print_cards Card.allDiamonds;
  print_newline ();
  print_endline "All Clubs:";
  print_cards Card.allClubs;
  print_newline ();

  let card1 = Card.newCard Card.Value.T7 Card.Color.Diamond in
  let card2 = Card.newCard Card.Value.T9 Card.Color.Heart in
  let card3 = Card.newCard Card.Value.As Card.Color.Club in
  let card4 = Card.newCard Card.Value.T2 Card.Color.Spade in
  print_endline (Card.toStringVerbose card1);
  print_endline ("has a value of " ^ (Card.Value.toStringVerbose (Card.getValue card1)));
  print_endline ("and is of type " ^ (Card.Color.toStringVerbose (Card.getColor card1)));
  print_newline ();

  let cards = [card1; card2; card3; card4] in
  print_endline "The best card is ";
  print_endline (Card.toStringVerbose (Card.best cards));
  print_newline ();

  let card = card3 in
  print_endline "Chosen card: ";
  print_endline (Card.toStringVerbose card);
  print_endline ("Is of type Spade? " ^ (if Card.isSpade card then "Yes" else "No"));
  print_endline ("Is of type Heart? " ^ (if Card.isHeart card then "Yes" else "No"));
  print_endline ("Is of type Diamond? " ^ (if Card.isDiamond card then "Yes" else "No"));
  print_endline ("Is of type Club? " ^ (if Card.isClub card then "Yes" else "No"))
