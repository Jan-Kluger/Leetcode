type 'a list_c = 
  | Node of 'a * 'a list_c
  | End

module Solution : sig
  val swap_pairs : 'a list_c -> 'a list_c
end = struct
  let rec swap_pairs (hd : 'a list_c) : 'a list_c =
    match hd with
    | Node(v1, Node(v2, n)) -> Node(v2, Node(v1, swap_pairs n))
    | n -> n
end

let test = (Node (1, (Node (2, (Node (3, (Node (4, (Node (5, End))))))))))

let res = Solution.swap_pairs test