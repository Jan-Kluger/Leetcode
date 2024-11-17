let move_zeros (ls : int list) =
  let p_fun num : bool = if num = 0 then true else false in
  let zeros, nums = List.partition (fun num -> p_fun num) ls in
  nums @ zeros

let test_input = [0;1;0;3;12]

let res = move_zeros test_input