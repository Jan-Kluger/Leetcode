module Solution = struct
  let uniqueOccurrences (arr : int list) : bool =
    match arr with
    | x :: xs -> true
    | _ -> false
end

let () =
  print_endline (string_of_bool (Solution.uniqueOccurrences [1;2;3]))