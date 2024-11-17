(* let increasingTriplet (nums : int list) =
  let rec in_helper (nums : int list) (acc : int option list) = 
    let curr = Option.get (List.hd acc)in
    match nums, acc with
    | _, [] -> true
    | [], _ -> false
    | x :: xs, y :: ys when x <= curr -> in_helper xs (Some x :: ys)
    | x :: xs, y :: ys when x > curr-> ([(List.hd acc)] :: [(in_helper xs)])
    | _, _ -> failwith "IMPOSSABLAL"
  in
in_helper (List.tl nums) [Some (List.hd nums); None; None]

let nums = [1;2;3;4;5]
let test = increasingTriplet nums *)
