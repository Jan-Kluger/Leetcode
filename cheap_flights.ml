module Solution : sig
  val findCheapestPrice : int -> int array array -> int -> int -> int -> int
end = struct
  let findCheapestPrice 
  (n : int) 
  (flights : int array array) 
  (src : int) 
  (dst : int) 
  (k : int) 
  : int =

  let dp = Array.init (k + 2) (fun _ -> Array.make n Int.max_int) in
  
  dp.(0).(src) <- 0;

  let rec parse (idx : int): unit =
    if idx >= (k + 1) then () else
    let k_idx = idx + 1 in
    dp.(k_idx).(src) <- 0;
    let rec update_dp (idx' : int) : unit =
      if idx' >= (Array.length flights) then
        ()
      else
        let u = flights.(idx').(0) in
        let v = flights.(idx').(1) in
        let cost = flights.(idx').(2) in
        let new_cost = if dp.(k_idx-1).(u) = Int.max_int then Int.max_int else dp.(k_idx-1).(u) + cost in
        dp.(k_idx).(v) <- min dp.(k_idx).(v) new_cost;
        update_dp (idx' + 1)
    in
    update_dp 0;
    parse (idx + 1)
  in
  parse 0;
  if dp.(k+1).(dst) = Int.max_int then (-1) else dp.(k+1).(dst)


end

let test_n = 4
let test_flights = [|[|0;1;100|];[|1;2;100|];[|2;0;100|];[|1;3;600|]|]
let test_src = 0
let test_dst = 3
let test_k = 1

let res = Solution.findCheapestPrice test_n test_flights test_src test_dst test_k
