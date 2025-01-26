type 'a b_tree = 
  | Node of 'a b_tree * 'a * 'a b_tree
  | Leaf

module Solution : sig
  val pre_order : int b_tree -> int list
end = struct
  let pre_order (tree : int b_tree) : int list =
    let rec traverse_helper (tree' : int b_tree) (acc : int list) : int list =
      match tree' with
      | Node (l, v ,r) -> traverse_helper r (traverse_helper l (v :: acc))
      | Leaf -> acc
    in
    traverse_helper tree [] |> List.rev
end

let test = Node (Node (Node (Leaf, 4, Leaf), 2, Node (Node (Leaf, 6, Leaf), 5, Node (Leaf, 7, Leaf))), 1, Node (Leaf, 3, Node (Node (Leaf, 9, Leaf), 8, Leaf)))

let res = Solution.pre_order test