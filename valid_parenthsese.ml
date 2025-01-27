module Solution : sig
  val check_valid : string -> bool
end = struct
  let pop_opt (stack : 'a list) : ('a option * 'a list) =
    match stack with
    | v :: vs -> (Some v, vs)
    | [] -> (None, [])

  let get_compliment = function
  | '[' -> ']'
  | '{' -> '}'
  | '(' -> ')'
  | _ -> failwith "most be parantheses"
  let check_valid (input : string) : bool =
    let str_in = String.to_seq input |> List.of_seq in
    let open_par = ['[';'{';'('] in
    let stack = [] in
    let rec parse (rest : char list) (stack : char list) : bool =
      match rest with
      | [] -> List.is_empty stack
      | v :: vs ->
        if List.mem v open_par then
          (* Open paranthese case *)
          parse vs (v :: stack)
        else
          (* Closed parentheses case *)
          let element, new_stack = pop_opt stack in
          begin match element with
          | None -> 
            (* If there are no parentheses in the stack, fail *)
            false
          | Some o when v = get_compliment o ->
            (* If top element in stack is complement, move on *)
            parse vs new_stack
          | _ -> 
            (* if last parentheeses is not complement, fail *)
            false
          end
    in
    parse str_in stack
end

let test = "()[]{}"

let res = Solution.check_valid test