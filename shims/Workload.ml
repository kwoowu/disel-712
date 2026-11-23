(* Seeded calculator workloads, shared by the live client and the simulator.

   A pool of [keys] requests is drawn first, each a list of [len] values in
   [0, vmax]; then [n] requests are drawn uniformly from the pool. [keys]
   therefore controls the memoizing server's hit rate and cache size. *)

let generate ~n ~len ~vmax ~keys ~seed =
  let st = Random.State.make [| seed |] in
  let pool =
    Array.init keys (fun _ -> List.init len (fun _ -> Random.State.int st (vmax + 1))) in
  List.init n (fun _ -> pool.(Random.State.int st keys))

(* One line per (request, result) pair, e.g. "1 2 -> 3". *)
let to_text pairs =
  let b = Buffer.create 4096 in
  List.iter (fun (a, v) ->
      Buffer.add_string b (String.concat " " (List.map string_of_int a));
      Buffer.add_string b " -> ";
      Buffer.add_string b (string_of_int v);
      Buffer.add_char b '\n') pairs;
  Buffer.contents b

let digest pairs = Digest.to_hex (Digest.string (to_text pairs))

(* Results cover exactly the requests, in order, and each result is f of its
   request (f = sum of the first two elements, as in SimpleCalculatorApp). *)
let check reqs pairs =
  List.map fst pairs = reqs
  && List.for_all (fun (a, v) ->
         match a with x :: y :: _ -> v = x + y | _ -> false) pairs
