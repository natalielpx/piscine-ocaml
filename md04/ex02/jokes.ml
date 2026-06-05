(* Forbidden functions : None *)

let jokes () =
  let arr = [|  "I was wondering why the frisbee kept getting bigger. Then it hit me.";
                "What do you call a pony with a sore-throat? A little hoarse.";
                "What starts with 'W' and ends with 'T'.";
                "My attention span is—oh look, a bird.";
                "I invented a new word: plagiarism."  |]
  in
  print_endline arr.(Random.int (Array.length arr))

let () =
  Random.self_init ();
  jokes ();
  jokes ();
  jokes ()