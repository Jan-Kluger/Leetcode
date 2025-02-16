module type UNIONFIND = sig
  type t
  val create : int -> t
  val find_set : t -> int -> int
  val union_set : t -> int -> int -> unit
end



module UnionFind : UNIONFIND = struct

  type uf_node = {
  parent : int array;  (* Parent of each element *)
  rank : int array;    (* Rank (approximate tree depth) of each set *)
  }

  type t = uf_node

  let create = failwith "todo"
  let find_set = failwith "todo"
  let union_set = failwith "todo"
end

module Solution (UF : UNIONFIND) : sig
  val findCircleNum : int array array -> int
end = struct
  let findCircleNum (graph : int array array) : int =
    failwith "todo"
end