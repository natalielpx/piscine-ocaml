(* Allowed functions : None *)

(** Create the type phosphate, which is an alias for the string type. *)
type phosphate = string

(** Create the type deoxyribose, which is also an alias for the string type. *)
type deoxyribose = string

(** Create the variant type nucleobase. Its constructors are A, T, C, G, and None. *)
type nucleobase = A | T | C | G | None

(** Write the nucleotide type that contains three elements:
  * one phosphate,
  * one deoxyribose,
  * one nucleobase.
  * The structure of the type nucleotide is up to you;
  * a record or a tuple will do the trick. *)
type nucleotide = ( phosphate * deoxyribose * nucleobase )

(** Write a function generate_nucleotide that
  * returns a nucleotide
  * from a given nucleobase passed as a char.
  * The function must be typed as
  * val generate_nucleotide : char -> nucleotide.
  * Set the phosphate value to "phosphate" and
  * the deoxyribose value to "deoxyribose". *)
let generate_nucleotide c = 
  let nucleo =
    match c with
    | 'A' -> A
    | 'T' -> T
    | 'C' -> C
    | 'G' -> G
    | _   -> None
  in
  ("phosphate", "deoxyribose", nucleo)

(* Demonstration Program *)

(** Prints a nucleotide *)
let print_nucleotide nucleo =
  let (ph, db, nb) = nucleo in
  match nb with
  | A     -> print_endline (ph ^ " " ^ db ^ " " ^ "A")
  | T     -> print_endline (ph ^ " " ^ db ^ " " ^ "T")
  | C     -> print_endline (ph ^ " " ^ db ^ " " ^ "C")
  | G     -> print_endline (ph ^ " " ^ db ^ " " ^ "G")
  | None  -> raise (Invalid_argument "nucleobase type missing")

let () =
  print_nucleotide (generate_nucleotide 'A');
  print_nucleotide (generate_nucleotide 'T');
  print_nucleotide (generate_nucleotide 'C');
  print_nucleotide (generate_nucleotide 'G');
  print_nucleotide (generate_nucleotide 'z');