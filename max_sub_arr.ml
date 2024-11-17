module Solution = struct
  (* get max elelemtn from a list *)
  let getMax (ls : 'a list) : 'a = 
    match ls with
    | x :: xs -> List.fold_left (fun acc num -> if num > acc then num else acc) x xs
    | _ -> failwith "empty list has no max element"

  (* take "amount" many elements from a list *)
  let rec take (ls : 'a list) (amount : int) : 'a list =
    match ls, amount with
    | x :: xs, 0 -> []
    | x :: xs, n -> x :: take xs (n-1)
    | [], _ -> []

  let rec gen_n_lists (ls : 'a list) (amount : int) : 'a list list = 
    if List.length ls <= amount then [take ls amount] else
      let rec helper h_ls =
        match h_ls with
        | _ :: xs when List.length h_ls >= amount -> (take h_ls amount) :: helper xs
        | _ -> []
      in
      helper ls
    
  (* calculate the subarr with highest avg *)
  let max_avg (ls : int list) (ws : int) = 
    (* catch possible division by 0 *)
    if ws = 0 then failwith "es can't be 0" else

    (* generate all possible k-lists and get average *)
    let lists = gen_n_lists ls ws in
    let avg_lists = List.map (fun sub_list -> List.fold_left (fun acc element -> acc +. (float_of_int element) /. float_of_int ws) 0.0 sub_list) lists in
    
    (* get max avg from list of averages *)
    getMax avg_lists
end

(* testing *)
let test_in = [1;12;-5;-6;50;3]

let test_take = Solution.max_avg test_in 4