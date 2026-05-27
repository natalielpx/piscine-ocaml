(* Allowed functions : None *)

(** Write a type rna as a list of elements of type nucleobase. *)
type rna = Nucleotide.nucleobase list

(** Write a function generate_rna that
  * creates an element of type rna
  * from an element of type helix
  * according to the following rules :
  * ◦ During the creation,
  * the rna is just like a complementary helix except that
  * the T nucleobase is switched to
  * U nucleobase (Uracil).
  * ◦ The list of nucleobases of the rna
  * is the list of nucleobases
  * that are complementary with the original helix's nucleobase
  * (except for the first rule).
  * For instance,
  * the sequence of nucleobase "ATCGA"
  * will produce a [U;A;G;C;U] rna.
  * The function must be typed as:
  * val generate_rna: helix -> rna. *)
let generate_rna helix =
  let rec aux comp list =
    match list with
    | [] -> comp
    | nucl :: tail ->
      let base =
        match nucl with
        | (_, _, Nucleotide.A) -> Nucleotide.U
        | (_, _, Nucleotide.T) -> Nucleotide.A
        | (_, _, Nucleotide.C) -> Nucleotide.G
        | (_, _, Nucleotide.G) -> Nucleotide.C
        | (_, _, _) -> assert false
      in
      aux (comp @ [base]) tail
  in
  aux [] helix 

(** Prints an rna *)
let print_rna rna =
  let rec aux tail =
    match tail with
    | [] -> print_newline ()
    | base :: t ->
      Nucleotide.print_nucleobase base;
      aux t
  in
  aux rna

let () =
  let h = Helix.generate_helix 16 in
  print_endline (Helix.helix_to_string h);
  print_rna (generate_rna h)