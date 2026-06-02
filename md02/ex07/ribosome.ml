(* Allowed functions : None *)

let rev lst =
  let rec aux acc = function
    | []        -> acc
    | l :: tail -> aux (l :: acc) tail
  in
  aux [] lst

(** Write a function generate_bases_triplets that
  * creates a list of triplets of 
  * elements of type nucleobase 
  * from an element of type rna 
  * according to the following rule:
  * if the number of nucleobases of the list 
  * is not a multiple of 3, 
  * it ignores the last incomplete triplet.
  * The function must be typed as :
  * generate_bases_triplets : rna -> (nucleobase * nucleobase * nucleobase) list. *)
let generate_bases_triplets rna =
  let rec aux acc = function
    | b1 :: b2 :: b3 :: tail -> aux ((b1, b2, b3) :: acc) tail
    | _ -> rev acc
  in
  aux [] rna

(** Prints a list of triplets *)
let print_triplets triplets =
  let rec aux = function
    | []                    -> print_newline ()
    | (b1, b2, b3) :: tail  ->
      Nucleotide.print_nucleobase b1;
      Nucleotide.print_nucleobase b2;
      Nucleotide.print_nucleobase b3;
      print_string " ";
      aux tail
  in
  aux triplets

type aminoacid =  Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val  

(* Write a protein type that consists of a list of aminoacid *)
type protein = aminoacid list

(* Write the function string_of_protein : protein -> string. *)
let string_of_protein protein =
    let rec aux str = function
    | []            -> str
    | Stop  :: tail -> aux ("Stop " ^ str) tail
    | Ala   :: tail -> aux ("Ala " ^ str) tail
    | Arg   :: tail -> aux ("Arg " ^ str) tail
    | Asn   :: tail -> aux ("Asn " ^ str) tail
    | Asp   :: tail -> aux ("Asp " ^ str) tail
    | Cys   :: tail -> aux ("Cys " ^ str) tail
    | Gln   :: tail -> aux ("Gln " ^ str) tail
    | Glu   :: tail -> aux ("Glu " ^ str) tail
    | Gly   :: tail -> aux ("Gly " ^ str) tail
    | His   :: tail -> aux ("His " ^ str) tail
    | Ile   :: tail -> aux ("Ile " ^ str) tail
    | Leu   :: tail -> aux ("Leu " ^ str) tail
    | Lys   :: tail -> aux ("Lys " ^ str) tail
    | Met   :: tail -> aux ("Met " ^ str) tail
    | Phe   :: tail -> aux ("Phe " ^ str) tail
    | Pro   :: tail -> aux ("Pro " ^ str) tail
    | Ser   :: tail -> aux ("Ser " ^ str) tail
    | Thr   :: tail -> aux ("Thr " ^ str) tail
    | Trp   :: tail -> aux ("Trp " ^ str) tail
    | Tyr   :: tail -> aux ("Tyr " ^ str) tail
    | Val   :: tail -> aux ("Val " ^ str) tail
  in
  aux "" (rev protein)

(** Write a function decode_arn of type rna -> protein that
  * creates a list of the variant type aminoacid 
  * from an element of type rna 
  * according to the following rules:

  * ◦ The decode process
  * begins with the first triplet and
  * ends with the first Stop triplet encountered.

  * ◦ Here is the matching table of
  * the nucleobases triplet,
  * the corresponding amino acid and
  * the constructor of type aminoacid :

  * UAA, UAG, UGA : End of translation -> Stop
  * GCA, GCC, GCG, GCU : Alanine -> Ala
  * AGA, AGG, CGA, CGC, CGG, CGU : Arginine -> Arg
  * AAC, AAU : Asparagine -> Asn
  * GAC, GAU : Aspartique -> Asp
  * UGC, UGU : Cysteine -> Cys
  * CAA, CAG : Glutamine -> Gln
  * GAA, GAG : Glutamique -> Glu
  * GGA, GGC, GGG, GGU : Glycine -> Gly
  * CAC, CAU : Histidine -> His
  * AUA, AUC, AUU : Isoleucine -> Ile
  * CUA, CUC, CUG, CUU, UUA, UUG : Leucine -> Leu
  * AAA, AAG : Lysine -> Lys
  * AUG : Methionine -> Met
  * UUC, UUU : Phenylalanine -> Phe
  * CCC, CCA, CCG, CCU : Proline -> Pro
  * UCA, UCC, UCG, UCU, AGU, AGC : Serine -> Ser
  * ACA, ACC, ACG, ACU : Threonine -> Thr
  * UGG : Tryptophane -> Trp
  * UAC, UAU : Tyrosine -> Tyr
  * GUA, GUC, GUG, GUU : Valine -> Val *)
let decode_arn rna =
  let trips = generate_bases_triplets rna in
  let rec aux acc = function
    | (Nucleotide.U, Nucleotide.A, Nucleotide.A) :: _
    | (Nucleotide.U, Nucleotide.A, Nucleotide.G) :: _
    | (Nucleotide.U, Nucleotide.G, Nucleotide.A) :: _    -> rev (Stop :: acc)
    | (Nucleotide.G, Nucleotide.C, Nucleotide.A) :: tail
    | (Nucleotide.G, Nucleotide.C, Nucleotide.C) :: tail
    | (Nucleotide.G, Nucleotide.C, Nucleotide.G) :: tail
    | (Nucleotide.G, Nucleotide.C, Nucleotide.U) :: tail -> aux (Ala :: acc) tail
    | (Nucleotide.A, Nucleotide.G, Nucleotide.A) :: tail
    | (Nucleotide.A, Nucleotide.G, Nucleotide.G) :: tail
    | (Nucleotide.C, Nucleotide.G, Nucleotide.A) :: tail
    | (Nucleotide.C, Nucleotide.G, Nucleotide.C) :: tail
    | (Nucleotide.C, Nucleotide.G, Nucleotide.G) :: tail
    | (Nucleotide.C, Nucleotide.G, Nucleotide.U) :: tail -> aux (Arg :: acc) tail
    | (Nucleotide.A, Nucleotide.A, Nucleotide.C) :: tail
    | (Nucleotide.A, Nucleotide.A, Nucleotide.U) :: tail -> aux (Asn :: acc) tail
    | (Nucleotide.G, Nucleotide.A, Nucleotide.C) :: tail
    | (Nucleotide.G, Nucleotide.A, Nucleotide.U) :: tail -> aux (Asp :: acc) tail
    | (Nucleotide.U, Nucleotide.G, Nucleotide.C) :: tail
    | (Nucleotide.U, Nucleotide.G, Nucleotide.U) :: tail -> aux (Cys :: acc) tail
    | (Nucleotide.C, Nucleotide.A, Nucleotide.A) :: tail
    | (Nucleotide.C, Nucleotide.A, Nucleotide.G) :: tail -> aux (Gln :: acc) tail
    | (Nucleotide.G, Nucleotide.A, Nucleotide.A) :: tail
    | (Nucleotide.G, Nucleotide.A, Nucleotide.G) :: tail -> aux (Glu :: acc) tail
    | (Nucleotide.G, Nucleotide.G, Nucleotide.A) :: tail
    | (Nucleotide.G, Nucleotide.G, Nucleotide.C) :: tail
    | (Nucleotide.G, Nucleotide.G, Nucleotide.G) :: tail
    | (Nucleotide.G, Nucleotide.G, Nucleotide.U) :: tail -> aux (Gly :: acc) tail
    | (Nucleotide.C, Nucleotide.A, Nucleotide.C) :: tail
    | (Nucleotide.C, Nucleotide.A, Nucleotide.U) :: tail -> aux (His :: acc) tail
    | (Nucleotide.A, Nucleotide.U, Nucleotide.A) :: tail
    | (Nucleotide.A, Nucleotide.U, Nucleotide.C) :: tail
    | (Nucleotide.A, Nucleotide.U, Nucleotide.U) :: tail -> aux (Ile :: acc) tail
    | (Nucleotide.C, Nucleotide.U, Nucleotide.A) :: tail
    | (Nucleotide.C, Nucleotide.U, Nucleotide.C) :: tail
    | (Nucleotide.C, Nucleotide.U, Nucleotide.G) :: tail
    | (Nucleotide.C, Nucleotide.U, Nucleotide.U) :: tail
    | (Nucleotide.U, Nucleotide.U, Nucleotide.A) :: tail
    | (Nucleotide.U, Nucleotide.U, Nucleotide.G) :: tail -> aux (Leu :: acc) tail
    | (Nucleotide.A, Nucleotide.A, Nucleotide.A) :: tail
    | (Nucleotide.A, Nucleotide.A, Nucleotide.G) :: tail -> aux (Lys :: acc) tail
    | (Nucleotide.A, Nucleotide.U, Nucleotide.G) :: tail -> aux (Met :: acc) tail
    | (Nucleotide.U, Nucleotide.U, Nucleotide.C) :: tail
    | (Nucleotide.U, Nucleotide.U, Nucleotide.U) :: tail -> aux (Phe :: acc) tail
    | (Nucleotide.C, Nucleotide.C, Nucleotide.C) :: tail
    | (Nucleotide.C, Nucleotide.C, Nucleotide.A) :: tail
    | (Nucleotide.C, Nucleotide.C, Nucleotide.G) :: tail
    | (Nucleotide.C, Nucleotide.C, Nucleotide.U) :: tail -> aux (Pro :: acc) tail
    | (Nucleotide.U, Nucleotide.C, Nucleotide.A) :: tail
    | (Nucleotide.U, Nucleotide.C, Nucleotide.C) :: tail
    | (Nucleotide.U, Nucleotide.C, Nucleotide.G) :: tail
    | (Nucleotide.U, Nucleotide.C, Nucleotide.U) :: tail
    | (Nucleotide.A, Nucleotide.G, Nucleotide.U) :: tail
    | (Nucleotide.A, Nucleotide.G, Nucleotide.C) :: tail -> aux (Ser :: acc) tail
    | (Nucleotide.A, Nucleotide.C, Nucleotide.A) :: tail
    | (Nucleotide.A, Nucleotide.C, Nucleotide.C) :: tail
    | (Nucleotide.A, Nucleotide.C, Nucleotide.G) :: tail
    | (Nucleotide.A, Nucleotide.C, Nucleotide.U) :: tail -> aux (Thr :: acc) tail
    | (Nucleotide.U, Nucleotide.G, Nucleotide.G) :: tail -> aux (Trp :: acc) tail
    | (Nucleotide.U, Nucleotide.A, Nucleotide.C) :: tail
    | (Nucleotide.U, Nucleotide.A, Nucleotide.U) :: tail -> aux (Tyr :: acc) tail
    | (Nucleotide.G, Nucleotide.U, Nucleotide.A) :: tail
    | (Nucleotide.G, Nucleotide.U, Nucleotide.C) :: tail
    | (Nucleotide.G, Nucleotide.U, Nucleotide.G) :: tail
    | (Nucleotide.G, Nucleotide.U, Nucleotide.U) :: tail -> aux (Val :: acc) tail
    | _                                                  -> raise (Invalid_argument "Stop amino acid not found")
  in
  aux [] trips

(* Demonstration Program *)
let () =
  (* Ala, Stop *)
  let rna1 = [Nucleotide.G; Nucleotide.C; Nucleotide.A; Nucleotide.U; Nucleotide.A; Nucleotide.A] in
  (* Met, Val, Stop *)
  let rna2 = [Nucleotide.A; Nucleotide.U; Nucleotide.G; Nucleotide.G; Nucleotide.U; Nucleotide.A; Nucleotide.U; Nucleotide.A; Nucleotide.G] in
  (* Phe, Lys, Gly, Stop *)
  let rna3 = [Nucleotide.U; Nucleotide.U; Nucleotide.C; Nucleotide.A; Nucleotide.A; Nucleotide.A; Nucleotide.G; Nucleotide.G; Nucleotide.U; Nucleotide.U; Nucleotide.G; Nucleotide.A] in
  (* Trp, His, Stop *)
  let rna4 = [Nucleotide.U; Nucleotide.G; Nucleotide.G; Nucleotide.C; Nucleotide.A; Nucleotide.C; Nucleotide.U; Nucleotide.G; Nucleotide.A] in
  (* Ser, Ile, Thr, Stop *)
  let rna5 = [Nucleotide.A; Nucleotide.G; Nucleotide.C; Nucleotide.A; Nucleotide.U; Nucleotide.A; Nucleotide.A; Nucleotide.C; Nucleotide.U; Nucleotide.U; Nucleotide.A; Nucleotide.A] in
  print_endline (string_of_protein (decode_arn rna1));
  print_endline (string_of_protein (decode_arn rna2));
  print_endline (string_of_protein (decode_arn rna3));
  print_endline (string_of_protein (decode_arn rna4));
  print_endline (string_of_protein (decode_arn rna5))