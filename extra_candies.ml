let find_max (list : int list) : int option = 
  if list = [] then None else
  (* no case for if empty list *)
  let rec find_helper (list : int list) (acc : int) =
    match list with
    | x :: xs ->  
      if x > acc then 
        find_helper xs x 
      else
        find_helper xs acc
    | [] -> acc
      in
    Some (find_helper list (List.nth list 0))

let calculate (candies : int list) (extra : int) = 
  let extra_list = List.map (fun can -> can + extra) candies in
  let max = find_max candies in
  match max with
  | Some n -> List.rev (List.fold_left (fun acc x -> if x >= Option.get max then true::acc else false::acc) [] extra_list)
  | _ -> []

let test_extra_candies = 1
let test_input = [4;2;1;1;2]
let test_res = calculate test_input test_extra_candies



