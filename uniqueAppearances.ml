module Solution = struct
  module IntMap = Map.Make(Int) 

  let uniqueOccurrences (arr : int list) : bool =
    let empty_map = IntMap.empty in
    let rec helper_oc (arr : int list) (map : int IntMap.t) =
      match arr with
      | x :: xs -> 
        let count = IntMap.find_opt x map |> Option.value ~default:0 in
        let new_map = IntMap.add x (count + 1) map in
        helper_oc xs new_map
      | [] -> 
        let values = IntMap.bindings map |> List.fold_left 
        (fun acc binding -> 
          match binding with
        | (k,v) -> acc ^ string_of_int k ^ " : " ^ string_of_int v ^ "\n") "" in
        Printf.printf "%s" values;
        failwith "todo"

    in
    helper_oc arr empty_map
end

let () =
  print_endline (string_of_bool (Solution.uniqueOccurrences [1;2;3]))