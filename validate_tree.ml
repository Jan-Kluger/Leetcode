type 'a b_tree = 
  | Node of 'a b_tree * 'a * 'a b_tree
  | Leaf

module Solution : sig
  val validate : int b_tree -> bool
end = struct
    
  let rec validate_bst tree low high =
    match tree with
    | Leaf -> true
    | Node (l, v, r) ->
      v > low && v < high
      && validate_bst l low v
      && validate_bst r v high

  let validate (tree : int b_tree) : bool =
    validate_bst tree Int.min_int Int.max_int 
  
end

let test = Node (Node (Leaf, 1, Leaf), 5, Node (Node (Leaf, 3, Leaf), 4, Node (Leaf, 6, Leaf)))

let res = Solution.validate test