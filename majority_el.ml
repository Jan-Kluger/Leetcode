module Solution : sig
  val majority_el : int array -> int
end = struct
  module IntMap = Map.Make(Int)
  let majority_el (arr : int array) = 
    let arr_len = Array.length arr in
    let rec parse idx (counter) (max_so_far : (int* int) option) = 
      if idx >= arr_len then 
        match max_so_far with
        | None -> -1
        | Some (n, count) -> n
      else 
        let element = arr.(idx) in
        let count = IntMap.find_opt element counter |> Option.value ~default:0 in

        if count > (arr_len/2) then
          element
        else
          let newCounter = IntMap.add element (count + 1) counter in
          let new_max = 
            match max_so_far with
            | None -> Some (element, count)
            | Some (n, o_count) -> 
              if o_count >= count then
                Some (n, o_count)
              else
                Some (element, count + 1)
              in
          parse (idx + 1) newCounter new_max
      in
      parse 0 (IntMap.empty) None
end

let test_arr = [|2;3;1;3|]

let res = Solution.majority_el test_arr