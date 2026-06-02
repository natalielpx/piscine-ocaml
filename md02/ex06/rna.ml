(* Allowed functions : None *)

(** Reverses a list *)
let rev lst =
  let rec aux acc = function
    | []        -> acc
    | l :: tail -> aux (l :: acc) tail
  in
  aux [] lst

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
  let rec aux comp = function
    | [] -> rev comp
    | (_, _, base) :: tail ->
      let nucl =
        match base with
        | Nucleotide.A    -> Nucleotide.U
        | Nucleotide.T    -> Nucleotide.A
        | Nucleotide.C    -> Nucleotide.G
        | Nucleotide.G    -> Nucleotide.C
        | Nucleotide.U    -> raise (Invalid_argument "Invalid nucleobase detected (U)")
        | Nucleotide.None -> raise (Invalid_argument "Invalid nucleobase detected (None)")
      in
      aux (nucl :: comp) tail
  in
  aux [] helix 

(** Prints an rna *)
let print_rna rna =
  let rec aux = function
    | []            ->  print_newline ()
    | base :: tail  ->  Nucleotide.print_nucleobase base;
                        aux tail
  in
  aux rna

let () =
  let h = Helix.generate_helix 16 in
  print_endline (Helix.helix_to_string h);
  print_rna (generate_rna h)