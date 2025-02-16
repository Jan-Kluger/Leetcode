module Solution : sig
  val find_max : int list -> int
end = struct
  let find_max (nums : int list) : int = 
    let res, _ = List.fold_left(fun (m, c) num -> 
        if c + num < 0 then
          (m, 0)
        else
          if c + num > m then
            (c + num, c + num)
          else
            (m, c + num)
      ) (0,0) nums in
    res
end

let test = [-2;1;-3;4;-1;2;1;-5;4]
(* let test = [5;4;-1;7;8] *)

let res = Solution.find_max test