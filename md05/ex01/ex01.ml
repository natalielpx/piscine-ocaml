(* Allowed functions : The Hashtbl module, String.length and String.get *)

module StringHash =
  struct
    type t = string
    let equal s1 s2 = s1=s2
    let hash str = String.length str
  end

module StringHashtbl = Hashtbl.Make(StringHash)

let () =
  let ht = StringHashtbl.create 5 in                                            (* Create a new hashtable of initial size 5 *)
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in                (* Transforms list of strings into list of (string, int) pairs *)
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;                      (* Iterates over pairs and inserts each into hashtable *)
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht   (* Prints each key-value pair *)