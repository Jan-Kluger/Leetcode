type trie =
  | Node of (char * trie list)

module type TRIE = sig
  type t

  val insert : t -> string -> t
  val search : t -> string -> bool
  val startswith : t -> string -> bool
  
end

module My_trie : TRIE with type t = trie list = struct
  type t = trie list

  let contains_node (elements : t) (element: char) : 'a option =
    List.map (fun (Node (v,_)) -> v) elements |> List.find_opt (fun ch -> ch = element)

  let insert (trie_list : t) (str : string) : t =
    let l_str = String.to_seq str |> List.of_seq in
    let rec insert_helper (trie_list' : t) (str : char list) : t = 
      match l_str with
      | x :: xs -> 
        (* if we have spomething to insert, then make new trie with x and insert it into children *)
        let node_to_insert = contains_node trie_list' x in
        begin match node_to_insert with 
        | Some v -> List.map (fun (Node (n, c)) -> if n = v then Node (n, insert_helper c xs) else Node (n, c)) trie_list'
        | None -> Node (x, insert_helper [] xs) :: trie_list'
        end
      (* if we have nothing to insert, just return our list *)
      | [] -> trie_list'
    in
    insert_helper trie_list l_str

  let search (trie : t) (str : string) : bool =
    failwith "todo"

  let startswith (trie : t) (str : string) : bool =
    failwith "todo"

end