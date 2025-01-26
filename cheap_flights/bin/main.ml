(* Define a priority queue using a binary heap *)
module PriorityQueue = struct
  type 'a t = {
    mutable heap : 'a array;
    cmp : 'a -> 'a -> int;
    mutable size : int;
  }

  let create cmp capacity =
    {
      heap = Array.make capacity (Obj.magic 0);
      cmp;
      size = 0;
    }

  let swap heap i j =
    let temp = heap.(i) in
    heap.(i) <- heap.(j);
    heap.(j) <- temp

  let parent i = (i - 1) / 2
  let left i = 2 * i + 1
  let right i = 2 * i + 2

  let ensure_capacity pq =
    if pq.size >= Array.length pq.heap then
      let new_heap = Array.make (2 * pq.size) (Obj.magic 0) in
      Array.blit pq.heap 0 new_heap 0 pq.size;
      pq.heap <- new_heap

  let push pq x =
    ensure_capacity pq;
    pq.heap.(pq.size) <- x;
    let rec heapify_up i =
      if i = 0 then ()
      else
        let p = parent i in
        if pq.cmp pq.heap.(i) pq.heap.(p) < 0 then (
          swap pq.heap i p;
          heapify_up p
        )
    in
    heapify_up pq.size;
    pq.size <- pq.size + 1

  let pop pq =
    if pq.size = 0 then
      None
    else
      let top = pq.heap.(0) in
      pq.size <- pq.size - 1;
      if pq.size > 0 then (
        pq.heap.(0) <- pq.heap.(pq.size);
        let rec heapify_down i =
          let l = left i in
          let r = right i in
          let smallest = ref i in
          if l < pq.size && pq.cmp pq.heap.(l) pq.heap.( !smallest) < 0 then
            smallest := l;
          if r < pq.size && pq.cmp pq.heap.(r) pq.heap.( !smallest) < 0 then
            smallest := r;
          if !smallest <> i then (
            swap pq.heap i !smallest;
            heapify_down !smallest
          )
        in
        heapify_down 0
      );
      Some top
end

(* Define a tuple type for the priority queue: (cost, current_city, stops) *)
type state = int * int * int

(* Comparator for the priority queue based on the cost *)
let state_cmp (cost1, _, _) (cost2, _, _) =
  compare cost1 cost2

(* Function to build the adjacency list *)
let build_graph n flights =
  let graph = Array.make n [] in
  List.iter (fun (fromi, toi, pricei) ->
    graph.(fromi) <- (toi, pricei) :: graph.(fromi)
  ) flights;
  graph

(* Main function to find the cheapest price *)
let find_cheapest_price n flights src dst k =
  let graph = build_graph n flights in
  let pq = PriorityQueue.create state_cmp (n * 2) in
  PriorityQueue.push pq (0, src, 0);  (* (cost, city, stops) *)
  
  (* To keep track of the minimum cost to reach a city with a certain number of stops *)
  let visited = Array.make n (Array.make (k + 2) max_int) in
  visited.(src).(0) <- 0;

  let rec loop () =
    match PriorityQueue.pop pq with
    | None -> -1
    | Some (cost, city, stops) ->
      if city = dst then
        cost
      else if stops > k then
        loop ()
      else
        let neighbors = graph.(city) in
        List.iter (fun (neighbor, price) ->
          let new_cost = cost + price in
          let new_stops = stops + 1 in
          if new_stops <= k + 1 && new_cost < visited.(neighbor).(new_stops) then (
            visited.(neighbor).(new_stops) <- new_cost;
            PriorityQueue.push pq (new_cost, neighbor, new_stops)
          )
        ) neighbors;
        loop ()
  in
  loop ()

(* Example usage *)
let () =
  let n1 = 4 in
  let flights1 = [(0,1,100); (1,2,100); (2,0,100); (1,3,600); (2,3,200)] in
  let src1 = 0 in
  let dst1 = 3 in
  let k1 = 1 in
  let result1 = find_cheapest_price n1 flights1 src1 dst1 k1 in
  Printf.printf "Example 1 Output: %d\n" result1;  (* Should print 700 *)

  let n2 = 3 in
  let flights2 = [(0,1,100); (1,2,100); (0,2,500)] in
  let src2 = 0 in
  let dst2 = 2 in
  let k2 = 1 in
  let result2 = find_cheapest_price n2 flights2 src2 dst2 k2 in
  Printf.printf "Example 2 Output: %d\n" result2;  (* Should print 200 *)

  let k3 = 0 in
  let result3 = find_cheapest_price n2 flights2 src2 dst2 k3 in
  Printf.printf "Example 3 Output: %d\n" result3;  (* Should print 500 *)
