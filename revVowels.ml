let revVowels (input_string : string) : string =
  (* list of all vowels *)
  let vowels = ['a';'A';'e';'E';'i';'I';'o';'O';'u';'U'] in
  
  (* char list of original string *)
  let l_string = List.of_seq (String.to_seq input_string) in
  
  (* vowels in the original string *)
  let vowel_list = List.fold_left ((fun (acc : char list) (char : char) : char list -> 
    if (List.mem char vowels) then char :: acc else acc)) [] l_string in

  (* replacement function *)
  let rec revVowel_helper (in_str : char list) (vowel_list : char list) (vowels : char list)(acc : char list) : char list =
  match in_str, vowel_list with
    | l :: ls, v :: vs when List.mem l vowels -> revVowel_helper ls vs vowels (v :: acc)
    | l :: ls, vs -> revVowel_helper ls vs vowels (l :: acc)
    | [], v::vs -> revVowel_helper [] vs vowels (v :: acc)
    | [], [] -> acc
  in
  let res' = revVowel_helper l_string vowel_list vowels [] in
  List.fold_left (fun acc ch -> (String.make 1 ch)^acc) "" res'

let in_string = "IceCreAm"
let res = revVowels in_string

let () =
    print_endline res
