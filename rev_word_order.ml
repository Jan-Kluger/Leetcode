let reverseWords (input_string : string) : string =
  let list_input = List.of_seq (String.to_seq input_string) in
  let word, words = List.fold_left (fun acc ch -> 
    if String.make 1 ch = " " then 
      match acc with
      | (curr_word, word_list) -> ("" , curr_word :: word_list)
    else
      match acc with
      | (curr_word, word_list) -> (curr_word ^ String.make 1 ch, word_list)
    ) ("", []) list_input in
  List.fold_left (fun acc w -> acc ^ " " ^ w) word words

let s = "the sky is blue"
let res = reverseWords s