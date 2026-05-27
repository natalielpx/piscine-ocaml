(* Allowed functions : String concatenation operator and Random module *)

(* Write an helix type that is a list of elements of type nucleotide. *)
type helix = Nucleotide.nucleotide list

(** Write a function generate_helix that
  * takes an int n as a parameter and
  * constructs a random sequence of nucleotides as
  * a list of size n.
  * The function must be typed as
  * val generate_helix : int -> helix. *)
let generate_helix n =
  let rec aux acc i =
    if i = 0 then acc
    else
      let base =
        match (Random.int 4) with
        | 0 -> 'A'
        | 1 -> 'T'
        | 2 -> 'C'
        | 3 -> 'G'
        | _ -> assert false
      in
      aux ([Nucleotide.generate_nucleotide base] @ acc) (i - 1)
  in
  aux [] n

(** Write a function helix_to_string that
  * converts a list of nucleotides as 
  * helix type resulting from the previous function
  * to a string of nucleobases.
  * The function must be typed as
  * val helix_to_string : helix -> string. *)
let helix_to_string lst =
  let rec aux acc l =
    match l with
    | [] -> acc
    | nucl :: tail -> 
      let n =
        match nucl with
        | (_, _, Nucleotide.A) -> "A"
        | (_, _, Nucleotide.T) -> "T"
        | (_, _, Nucleotide.C) -> "C"
        | (_, _, Nucleotide.G) -> "G"
        | (_, _, _) -> assert false
      in
      aux (acc ^ n) tail
  in
  aux "" lst

(** Write a function complementary_helix that
  * takes an helix as a parameter and
  * generates the corresponding helix according to
  * the Nucleobase pairing rules that follows :
  * ◦ A (Adenine) can be associated with T.
  * ◦ T (Thymine) can be associated with A.
  * ◦ C (Cytosine) can be associated with G.
  * ◦ G (Guanine) can be associated with C.
  * The function must be typed as:
  * val complementary_helix : helix -> helix *)
let complementary_helix helix =
  let rec aux comp list =
    match list with
    | [] -> comp
    | nucl :: tail ->
      let base =
        match nucl with
        | (_, _, Nucleotide.A) -> 'T'
        | (_, _, Nucleotide.T) -> 'A'
        | (_, _, Nucleotide.C) -> 'G'
        | (_, _, Nucleotide.G) -> 'C'
        | (_, _, _) -> assert false
      in
      aux (comp @ [(Nucleotide.generate_nucleotide base)]) tail
  in
  aux [] helix 

let () =
  let h = generate_helix 16 in
  print_endline (helix_to_string h);
  print_endline (helix_to_string (complementary_helix h))