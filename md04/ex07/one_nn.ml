(* Forbidden functions : None *)

let one_nn lst ((f, c) : float list * string) =
  
  let len = List.length f in
  let rec aux min res = function
    | [] -> if min >= 0. then res else raise (Invalid_argument "No valid examples in database")
    | (flts, cls) :: tl ->
      if (Array.length flts) = len
      then
        let dist = Dist.euclidian (Array.of_list f) flts in
        if min = -1. || dist < min
        then aux dist cls tl
        else aux min res tl
      else
        aux min res tl
  in
  aux (-1.) "" lst

let () =
  let lst = Extract.extract "./ionosphere.train.csv" in
  let r1 = ([1.;0.;1.;-0.14236;1.;-0.16256;1.;-0.23656;1.;-0.07514;1.;-0.25010;1.;-0.26161;1.;-0.21975;1.;-0.38606;1.;-0.46162;1.;-0.35519;1.;-0.59661;1.;-0.47643;0.98820;-0.49687;1.;-0.75820;1.;-0.75761;1.;-0.84437], "?") in
  let r2 = ([1.;0.;0.68198;-0.17314;0.82332;0.21908;0.46643;0.32862;0.25795;0.58304;1.;-0.15194;0.01060;0.44523;0.01060;0.38869;0.18681;0.41168;0.10567;0.36353;0.04325;0.30745;-0.00083;0.24936;-0.02862;0.19405;-0.04314;0.14481;-0.04779;0.10349;-0.04585;0.07064;-0.04013;0.04586], "?") in
  let r3 = ([1.;0.;0.68198;-0.17314;0.82332;0.21;0.46;0.32862;0.25795;0.58;1.;-0.15194;0.01060;0.44523;0.01060;0.38869;0.18681;0.41168;0.10567;0.36353;0.04325;0.30745;-0.00083;0.24936;-0.02862;0.19;-0.04314;0.14481;-0.04779;0.10349;-0.04585;0.07064;-0.04013;0.04586], "?") in
  let r4 = ([9.], "?") in
  print_endline (one_nn lst r1);
  print_endline (one_nn lst r2);
  print_endline (one_nn lst r3);
  print_endline (one_nn lst r4)
