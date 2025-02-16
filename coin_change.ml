module Solution : sig
  val coin_change : int array -> int -> int
end = struct
  let coin_change (coins : int array) (sum : int) : int = 
    let max = sum + 1 in
    let dp = Array.make max max in

    (* Base case *)
    dp.(0) <- 0;

    let rec parse (idx : int) : unit =
      if idx > sum then () else begin
        let coins = Array.fold_left (fun acc x -> if x <= idx then (x :: acc) else acc) [] coins in
        let rec min_coin ?(minimum : int = dp.(idx)) (valid_coins : int list) : int = 
          match valid_coins with
          | x :: xs -> 
            let new_min = min minimum (dp.(idx - x) + 1) in
            min_coin ~minimum:new_min xs
          | [] -> minimum
        in
        dp.(idx) <- min_coin coins;
        parse (idx + 1)
      end
    in
    parse 1;
    dp.(sum)
end

let test_coins = [|1;2;5|]
let test_amount = 11

let res = Solution.coin_change test_coins test_amount