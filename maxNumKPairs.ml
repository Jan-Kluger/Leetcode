module IntMap = Map.Make(Int)

let make_table ls =
  let rec helper ls tbl =
    match ls with
    | x :: xs ->
      let count = match IntMap.find_opt x tbl with
        | Some c -> c
        | None -> 0
      in
      let new_tbl = IntMap.add x (count + 1) tbl in
      helper xs new_tbl
    | [] -> tbl
  in
  helper ls IntMap.empty

let map_to_string (key_to_string : 'k -> string) (value_to_string : 'v -> string) (map : 'k IntMap.t) : string =
  IntMap.fold (fun key value acc ->
    let kv_string = key_to_string key ^ " -> " ^ value_to_string value in
    if acc = "" then kv_string else acc ^ "\n" ^ kv_string
  ) map ""

  let count_pairs_with_sum_k (map : int IntMap.t) (k : int) : int =
    let sorted_keys = List.sort compare (IntMap.bindings map) in
    let rec helper keys pair_count =
      match keys with
      | [] -> pair_count  (* If we reach the end of the list, return the pair count *)
      | (first_key, first_count) :: rest ->
        let rec find_pairs keys pair_count =
          match keys with
          | [] -> pair_count  (* If no more elements to pair with, return the current pair count *)
          | (second_key, second_count) :: xs ->
            let sum = first_key + second_key in
            if sum = k then
              if first_key = second_key then
                (* Special case: first_key + first_key = k, form as many pairs as possible *)
                let pair_num = first_count / 2 in
                let updated_pair_count = pair_count + pair_num in
                let remaining_first = first_count mod 2 in
                if remaining_first > 0 then
                  helper ((first_key, remaining_first) :: rest) updated_pair_count
                else
                  helper rest updated_pair_count
              else
                (* Regular case: first_key + second_key = k *)
                let pair_num = min first_count second_count in
                let updated_pair_count = pair_count + pair_num in
                let updated_rest = if second_count - pair_num > 0 then (second_key, second_count - pair_num) :: xs else xs in
                find_pairs updated_rest updated_pair_count
            else if sum > k then
              pair_count  (* No need to check further, as the elements are sorted *)
            else
              (* If sum is less than k, continue checking the next element *)
              find_pairs xs pair_count
        in
        let updated_pair_count = find_pairs rest pair_count in
        helper rest updated_pair_count
    in
    helper sorted_keys 0
  

let nums1 = [3;1;3;4;3]

let int_int_toString = map_to_string string_of_int string_of_int 

let k = 6
let table = make_table nums1

let () =
  print_endline (int_int_toString table)

let res = count_pairs_with_sum_k table k

let () =
  print_endline (string_of_int res)