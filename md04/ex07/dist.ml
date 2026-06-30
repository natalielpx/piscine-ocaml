let euclidian arr1 arr2 =
  let n = Array.length arr1 in
  let rec aux acc i =
    if i < n
    then aux (acc +. ((arr1.(i) -. arr2.(i)) ** 2.)) (i + 1)
    else (sqrt acc)
  in
  aux 0. 0