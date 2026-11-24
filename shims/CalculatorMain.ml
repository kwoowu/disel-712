open Util
open Debug
open Shim

type mode = Client | Server

let mode : mode option ref = ref None
let server_name : State.nid option ref = ref None
let me : State.nid option ref = ref None
let nodes : (State.nid * (string * int)) list ref = ref []

(* Workload (client only). Without -n the client runs the original c_runner
   on the hardcoded client_input. *)
let wl_n : int option ref = ref None
let wl_len = ref 2
let wl_vmax = ref 100
let wl_keys = ref 100
let wl_seed = ref 0
let csv_file : string option ref = ref None
let out_file : string option ref = ref None

let usage msg =
  print_endline msg;
  Printf.printf "%s usage:\n" Sys.argv.(0);
  Printf.printf "    %s [OPTIONS] <CLUSTER>\n" (Array.get Sys.argv 0);
  print_endline "where:";
  print_endline "    CLUSTER   is a list of triples of ID IP_ADDR PORT,";
  print_endline "              giving all the nodes in the system";
  print_newline ();
  print_endline "Options are as follows:";
  print_endline "    -me <n>        the identity of this node (required)";
  print_endline "    -mode <MODE>   whether this node is the server or client (required)";
  print_endline "    -server <n>    the identity of the server (ignored)";
  print_endline "Client workload options:";
  print_endline "    -n <n>         number of requests (default: run the hardcoded client_input)";
  print_endline "    -len <n>       values per request, >= 2 (default 2)";
  print_endline "    -vmax <n>      values are drawn from [0, vmax] (default 100)";
  print_endline "    -keys <n>      number of distinct requests to draw from (default 100)";
  print_endline "    -seed <n>      random seed (default 0)";
  print_endline "    -csv <file>    append a result row to this CSV file";
  print_endline "    -out <file>    write the (request, result) pairs to this file";
  exit 1

let rec parse_args = function
  | [] -> ()
  | "-mode" :: "server" :: args -> mode := Some Server; parse_args args
  | "-mode" :: "client" :: args -> mode := Some Client; parse_args args
  | "-me" :: name :: args -> me := Some (nat_of_string name); parse_args args
  | "-server" :: name :: args -> server_name := Some (nat_of_string name); parse_args args
  | "-n" :: n :: args -> wl_n := Some (int_of_string n); parse_args args
  | "-len" :: n :: args -> wl_len := int_of_string n; parse_args args
  | "-vmax" :: n :: args -> wl_vmax := int_of_string n; parse_args args
  | "-keys" :: n :: args -> wl_keys := int_of_string n; parse_args args
  | "-seed" :: n :: args -> wl_seed := int_of_string n; parse_args args
  | "-csv" :: f :: args -> csv_file := Some f; parse_args args
  | "-out" :: f :: args -> out_file := Some f; parse_args args
  | name :: ip :: port :: args when name.[0] <> '-' ->
      let n = nat_of_string name in
      nodes := (n, (ip, int_of_string port)) :: !nodes;
      parse_args args
  | arg :: _ -> usage ("Unknown argument " ^ arg)

(* Nearest-rank percentile of a sorted array. *)
let percentile sorted p =
  let n = Array.length sorted in
  if n = 0 then nan
  else sorted.(max 0 (int_of_float (ceil (p *. float_of_int n)) - 1))

let run_workload n =
  let reqs = Workload.generate ~n ~len:!wl_len ~vmax:!wl_vmax ~keys:!wl_keys ~seed:!wl_seed in
  (* Convert to the extracted nat representation (int or unary) outside the
     timed region, and the results back after it. *)
  let nreqs = List.map (List.map nat_of_int) reqs in
  (* prec (at least two values per request) is the verified client's precondition *)
  if not (List.for_all SimpleCalculatorApp.prec nreqs) then usage "-len must be >= 2";
  Shim.record_latency := true;
  let t0 = Unix.gettimeofday () in
  let nres = SimpleCalculatorApp.compute_input nreqs in
  let elapsed = Unix.gettimeofday () -. t0 in
  let res = List.map (fun (a, v) -> (List.map int_of_nat a, int_of_nat v)) nres in
  let lat = Array.of_list (List.rev_map (fun s -> s *. 1e6) !Shim.latencies) in
  Array.sort compare lat;
  let digest = Workload.digest res in
  let ok = Workload.check reqs res in
  let fields = [
    string_of_int n; string_of_int !wl_len; string_of_int !wl_vmax;
    string_of_int !wl_keys; string_of_int !wl_seed;
    Printf.sprintf "%.6f" elapsed;
    Printf.sprintf "%.1f" (float_of_int n /. elapsed);
    Printf.sprintf "%.1f" (percentile lat 0.50);
    Printf.sprintf "%.1f" (percentile lat 0.90);
    Printf.sprintf "%.1f" (percentile lat 0.99);
    Printf.sprintf "%.1f" (percentile lat 1.0);
    digest; string_of_bool ok ] in
  let header = ["n"; "len"; "vmax"; "keys"; "seed"; "elapsed_s"; "throughput_rps";
                "p50_us"; "p90_us"; "p99_us"; "max_us"; "digest"; "ok"] in
  List.iter2 (fun h f -> Printf.printf "%s=%s " h f) header fields;
  print_newline ();
  (match !csv_file with
   | None -> ()
   | Some f ->
      let fresh = not (Sys.file_exists f) in
      let oc = open_out_gen [Open_append; Open_creat] 0o644 f in
      if fresh then output_string oc (String.concat "," header ^ "\n");
      output_string oc (String.concat "," fields ^ "\n");
      close_out oc);
  (match !out_file with
   | None -> ()
   | Some f -> let oc = open_out f in output_string oc (Workload.to_text res); close_out oc);
  if not ok then exit 1

let main () =
  (* Exit cleanly on SIGTERM so at_exit handlers (ocamlprof dump, Timed) run. *)
  Sys.set_signal Sys.sigterm (Sys.Signal_handle (fun _ -> exit 0));
  parse_args (List.tl (Array.to_list Sys.argv));
  match !mode, !me with
  | Some mode, Some me -> begin
     Shim.setup { nodes = !nodes; me = me; st = SimpleCalculatorApp.init_state };
     match mode with
     | Server ->
        begin match int_of_nat me with
          | 1 -> SimpleCalculatorApp.s_runner1 ()
          | 3 -> SimpleCalculatorApp.s_runner2 ()
          | n -> usage ("unknown server name " ^ string_of_int n)
        end
     | Client ->
        begin match !wl_n with
        | Some n -> run_workload n
        | None ->
           let l = SimpleCalculatorApp.c_runner () in
           Printf.printf "client got result list %a\n%!" (print_list (print_pair (print_list print_nat) print_nat)) l
        end
    end
  | _, _ -> usage "-mode and -me must be given"

let _ = main ()
