let findDifference (l1 : int list) (l2 : int list) : (int list list) =
  let l_l_diff listl listr = List.filter (fun num -> not (List.mem num listl)) listr in [l_l_diff l2 l1; l_l_diff l1 l2]

let nums_1 = [1;2;3]
let nums_2 = [2;4;6]

let res = findDifference nums_1 nums_2