module IntMap = Map.Make(Int)

module Solution = struct
  let singleNumber (ls : int list) =
    let rec helper (ls : int list) (map : int IntMap.t) =
      match ls with
      | x :: xs -> 
        let new_map = match IntMap.find_opt x map with
          | Some _ -> IntMap.remove x map
          | None -> IntMap.add x 1 map
        in
        helper xs new_map
      | [] -> 
        IntMap.bindings map
    in
    let getres (ls : (int * int) list) : (int * int) =
      match ls with
      | x :: xs -> x
      | [] -> failwith "should not be possible"
    in
    let res' = helper ls IntMap.empty
  in
  let res, _ = getres res' in
  res
end

let test_in = [4; 1; 2; 1; 2]

let final = Solution.singleNumber test_in