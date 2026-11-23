(* Time breakdown for a live node, enabled by DISEL_TIMING=<file>.
   Buckets:
     idle        - blocked in select waiting for a message
     network     - accept/recv/send and (de)serialization
     transitions - protocol transitions run by the shim (msg_wf, receive_step,
                   send_step) and the protocol-state update
     program     - everything else: the extracted program itself plus the
                   remaining shim glue (total minus the three buckets above)
   The report is written at exit (normal exit or SIGTERM -> exit 0). *)

let file = Sys.getenv_opt "DISEL_TIMING"
let enabled = file <> None

let now () = if enabled then Unix.gettimeofday () else 0.0

let idle = ref 0.0
let network = ref 0.0
let transitions = ref 0.0

(* Start of the current wait in select, if any (a node is usually blocked in
   select when SIGTERM arrives, so the report closes the open interval). *)
let idle_since : float option ref = ref None
let enter_idle () = if enabled then idle_since := Some (Unix.gettimeofday ())
let leave_idle () =
  match !idle_since with
  | Some t -> idle := !idle +. (Unix.gettimeofday () -. t); idle_since := None
  | None -> ()
let add_network d = network := !network +. d
let add_transitions d = transitions := !transitions +. d

let start = Unix.gettimeofday ()

let report f =
  leave_idle ();
  let total = Unix.gettimeofday () -. start in
  let program = total -. !idle -. !network -. !transitions in
  let oc = open_out f in
  let line name v =
    Printf.fprintf oc "%-12s %12.6f s %6.2f %%\n" name v (100.0 *. v /. total) in
  line "program" program;
  line "transitions" !transitions;
  line "network" !network;
  line "idle" !idle;
  line "total" total;
  close_out oc

let () = match file with Some f -> at_exit (fun () -> report f) | None -> ()
