(* Allowed functions : None *)

(** Create the type phosphate, which is an alias for the string type. *)
type phosphate = string

(** Create the type deoxyribose, which is also an alias for the string type. *)
type deoxyribose = string

(** Create the variant type nucleobase. Its constructors are A, T, C, G, and None. *)
type nucleobase = A
                | T
                | C
                | G
                | None

(** Write the nucleotide type that contains three elements:
  * one phosphate,
  * one deoxyribose,
  * one nucleobase.
  * The structure of the type nucleotide is up to you;
  * a record or a tuple will do the trick. *)
type nucleotide = ( phosphate *
                    deoxyribose *
                    nucleobase )

(** Write a function generate_nucleotide that
  * returns a nucleotide
  * from a given nucleobase passed as a char.
  * The function must be typed as
  * val generate_nucleotide : char -> nucleotide.
  * Set the phosphate value to "phosphate" and
  * the deoxyribose value to "deoxyribose". *)
let generate_nucleotide c = 
  let nucleo : nucleobase =
    match c with
    | 'A' -> A
    | 'T' -> T
    | 'C' -> C
    | 'G' -> G
    | _ -> None
  in
  ("phosphate", "deoxyribose", nucleo)

(* Prints a nucleotide *)
let print_nucleotide nucleo =
  match nucleo with (ph, db, nb) ->
  print_endline (ph);
  print_endline (db);
  match nb with
  | A -> print_endline ("A")
  | T -> print_endline ("T")
  | C -> print_endline ("C")
  | G -> print_endline ("G")
  | None -> print_endline ("None")
