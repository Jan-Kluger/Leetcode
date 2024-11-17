let rec divisor_of_string string divisor original_divisor =
  match string, divisor with
  | [], _ -> true
  | h1 :: t1, h2 :: t2 when h1 = h2 -> divisor_of_string t1 t2 original_divisor
  | h1 :: t1, [] -> divisor_of_string t1 original_divisor original_divisor
  | _, _ -> false

let largestGCD (string : string) (divisor : string) = 
  (* convert both lists to strings *)
  let list_string = List.of_seq(String.to_seq string) in
  let divisor_string = List.of_seq(String.to_seq divisor) in

  (* check if some part of divisor is gcd *)
  let rec largestGCD_helper (str_list : char list) (divisor_list : char list) : string =
    let div_bool = divisor_of_string str_list divisor_list divisor_list in
    match list_string, divisor_string, div_bool with
    | _, div, true -> String.of_seq(List.to_seq div)
    | _, _ :: new_div, _ -> largestGCD_helper str_list new_div
    | _, _, _  -> ""
  in 
  largestGCD_helper list_string divisor_string

let first_string = "ABCABC"
let div = "ABC"

let l_gcd = largestGCD first_string div

let () = 
  print_endline l_gcd
