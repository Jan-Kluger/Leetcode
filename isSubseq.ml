module Solution = struct
  let isSubsequence (str : string) (sub : string) : bool =
    let l_str = List.of_seq (String.to_seq str) in
    let l_sub = List.of_seq (String.to_seq sub) in
    let rec isSubHelper (str : char list) (sub : char list) : bool =
      match str, sub with
        | str_letter :: ls, sub_letter :: subs when str_letter = sub_letter -> isSubHelper ls subs
        | h :: t, _ -> isSubHelper t sub
        | _, [] -> true
        | [], _ -> false
      in
    isSubHelper l_str l_sub
end

let subseq = "axc"
let test_input = "ahbgdc"

let res = Solution.isSubsequence test_input subseq

let () =
  if res = true then
    print_endline "true"
  else
    print_endline "false"