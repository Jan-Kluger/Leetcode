module Solution : sig
  val sub_sum : int array -> int -> int
end = struct
  let sub_sum (values : int array) (k : int) : int = 
    let v_len = Array.length values in
    let counter = Hashtbl.create v_len in
    let rec make_cumu_sum ?(v_count : int = 0) (idx : int) =
      if idx > (v_len-1) then
        v_count
      else begin
        let new_val = 
          if idx = 0 then values.(idx) else
          values.(idx) + values.(idx-1) in
        let new_count = 
          match Hashtbl.find_opt counter (new_val - k) with
          | None -> v_count
          | Some _ -> v_count + 1
        in
        values.(idx) <- new_val;
        Hashtbl.add counter (values.(idx)) idx;
        make_cumu_sum (idx+1) ~v_count:new_count
      end
    in
    Hashtbl.add counter 0 0;
    make_cumu_sum 0;
end

let test = [|1;1;1|]

let k = 2

let res = Solution.sub_sum test k