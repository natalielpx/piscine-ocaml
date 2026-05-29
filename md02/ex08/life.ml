(* Allowed functions : String module *)

let rev lst =
  let rec aux acc = function
    | []        -> acc
    | l :: tail -> aux (l :: acc) tail
  in
  aux [] lst

(** Write a function that goes from
  * the generation of a helix of a reasonable length to
  * the creation of the corresponding protein.
  * Each step must be displayed clearly
  * on the standard output.
  * This function takes a string as a parameter. *)
let life str =
  print_endline ("Target: " ^ str);
  (* Convert each character into helix of corresponding nucleotides *)
  let len = String.length str in
  let helix =
    let rec aux acc i =
      if i >= len
      then rev acc
      else
        let nucl =
          match str.[i] with
          | 'A' -> Nucleotide.A
          | 'T' -> Nucleotide.T
          | 'C' -> Nucleotide.C
          | 'G' -> Nucleotide.G
          | _   -> raise (Invalid_argument "Invalid nucleobase type")
        in
        aux (("phosphate", "deoxyribose", nucl) :: acc) (i + 1)
    in
    aux [] 0
  in
  print_endline ("Helix:  " ^ Helix.helix_to_string helix);
  (* Create corresponding rna *)
  let rna = Rna.generate_rna helix in
  print_string ("Rna:    ");
  Rna.print_rna rna;
  (* Create corresponding protein *)
  print_string ("Trips:  ");
  Ribosome.print_triplets (Ribosome.generate_bases_triplets rna);
  Ribosome.decode_arn rna
  
(* Demonstration Program *)

let () =
  (* DNA: TACCAAATT -> RNA: AUG GUU UAA -> Met, Val, Stop *)
  print_endline("Result: " ^Ribosome.string_of_protein (life "TACCAAATT"));
  print_newline ();
  (* DNA: AAGTTTCCAACT -> RNA: UUC AAA GGU UGA -> Phe, Lys, Gly, Stop *)
  print_endline("Result: " ^Ribosome.string_of_protein (life "AAGTTTCCAACT"));
  print_newline ();
  (* DNA: ACCGTGACT -> RNA: UGG CAC UGA -> Trp, His, Stop *)
  print_endline("Result: " ^Ribosome.string_of_protein (life "ACCGTGACT"));
  print_newline ();
  (* DNA: TCGTATTGAATT -> RNA: AGC AUA ACU UAA -> Ser, Ile, Thr, Stop *)
  print_endline("Result: " ^Ribosome.string_of_protein (life "TCGTATTGAATT"));
  print_newline ()