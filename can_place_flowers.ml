let canPlaceflowers f fl =
  let rec planterhelper f fl p_f=
    match f, p_f with
    | x1 :: x2 :: xs, true when x1 = 0 && x2 = 0 -> planterhelper xs (fl - 1) true
    | x :: xs, _ when x = 1 -> planterhelper xs fl false
    | x :: xs, _ when x = 0 -> planterhelper xs fl true
    | [], _ -> if fl = 0 then true else false
    | _ -> failwith "shouldnt happen"
  in
  planterhelper f fl true

let f = [1;0;0;0;1]
let fl = 2

let res = canPlaceflowers f fl
