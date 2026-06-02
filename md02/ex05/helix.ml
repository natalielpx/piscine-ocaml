(* Allowed functions : String concatenation operator and Random module *)

(** Reverses a list *)
let rev lst =
  let rec aux acc = function
    | []        -> acc
    | l :: tail -> aux (l :: acc) tail
  in
  aux [] lst

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
      aux ((Nucleotide.generate_nucleotide base) :: acc) (i - 1)
  in
  aux [] n

(** Write a function helix_to_string that
  * converts a list of nucleotides as 
  * helix type resulting from the previous function
  * to a string of nucleobases.
  * The function must be typed as
  * val helix_to_string : helix -> string. *)
let helix_to_string lst =
  let rec aux acc = function
    | [] -> acc
    | (_, _, base) :: tail -> 
      let char =
        match base with
        | Nucleotide.A    -> "A"
        | Nucleotide.T    -> "T"
        | Nucleotide.C    -> "C"
        | Nucleotide.G    -> "G"
        | Nucleotide.None -> "?"
      in
      aux (acc ^ char) tail
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
  let rec aux comp = function
    | [] -> rev comp
    | (_, _, nucl) :: tail -> 
      let base =
        match nucl with
        | Nucleotide.A    -> 'T'
        | Nucleotide.T    -> 'A'
        | Nucleotide.C    -> 'G'
        | Nucleotide.G    -> 'C'
        | Nucleotide.None -> raise (Invalid_argument "Invalid nucleobase detected (None)")
      in
      aux ((Nucleotide.generate_nucleotide base) :: comp) tail
  in
  aux [] helix 

let () =
  let h = generate_helix 16 in
  print_endline (helix_to_string h);
  print_endline (helix_to_string (complementary_helix h))