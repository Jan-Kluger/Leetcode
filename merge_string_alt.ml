let mergealt (s1 : string) (s2 : string) = 
  let rec mergehelper (l1 : char list) (l2 : char list) : char list =
    match l1 , l2 with
    | [] , [] -> []
    | [] , l -> l
    | l , [] -> l
    | h1 :: t1, h2 :: t2 -> h1 :: h2 :: mergehelper t1 t2
    in
  let l1 = List.of_seq (String.to_seq s1) in
  let l2 = List.of_seq (String.to_seq s2) in
  let merge_list = mergehelper l1 l2 in
  List.fold_left (fun acc char -> acc ^ String.make 1 char) "" merge_list


let test_string_0 = "abc"
let test_string_1 = "efg"

let test_merge = mergealt test_string_0 test_string_1

let () = 
  print_endline test_merge
