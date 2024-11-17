let productExceptSelf (input : int list) =
  let prod = List.fold_left (fun acc num -> if num = 0 then acc * 1 else acc * num) 1 input in
  List.map (fun num -> if List.mem 0 input && not (num = 0) then 0 else if num = 0 then prod else prod/num) input

let in_test = [1;2;3;4]

let res = productExceptSelf in_test