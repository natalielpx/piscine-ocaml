(* Allowed functions : The Set module *)

module Strings =
  struct
    type t = string
    let compare s1 s2 = 
      if      s1 = s2 then  0
      else if s1 < s2 then  -1
      else                  1
  end

module StringSet = Set.Make(Strings)

let () =
  let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
  StringSet.iter print_endline set;
  print_endline (StringSet.fold ( ^ ) set "")