module RecentCounter : sig
  type t

  val create : unit -> t
  val ping : t -> int -> (int * t)
end = struct
  type t = int list

  let create () = []

  let ping requests t =
    let rec filter_recent acc = function
      | [] -> List.rev acc
      | x :: xs ->
        if x >= t - 3000 then filter_recent (x :: acc) xs
        else filter_recent acc xs
    in
    let recent_requests = filter_recent [] requests in
    let updated_requests = recent_requests @ [t] in
    (List.length updated_requests, updated_requests)
end
