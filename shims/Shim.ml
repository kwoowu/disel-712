open Unix
open Util
open Debug

(* Per-message logging, enabled by setting DISEL_LOG. *)
let log = Sys.getenv_opt "DISEL_LOG" <> None

(* Request/response latency recording, used by the client only (one request
   outstanding at a time, so each response pairs with the last request). *)
let record_latency = ref false
let last_request_sent = ref 0.0
let latencies : float list ref = ref []

let read_fds : (Unix.file_descr, int) Hashtbl.t = Hashtbl.create 17
let write_fds : (int, Unix.file_descr) Hashtbl.t = Hashtbl.create 17

type cfg = { nodes : (int * (string * int)) list
           ; me : int
           ; mutable st : State.StateGetters.state
           }

let the_cfg : cfg option ref = ref None
let listen_fd : file_descr = socket PF_INET SOCK_STREAM 0

let get_addr_port cfg name =
  try List.assoc name cfg.nodes
  with Not_found -> failwith (Printf.sprintf "Unknown name: %d" (int_of_nat name))

let get_name_for_read_fd fd =
  Hashtbl.find read_fds fd

let send_chunk (fd : file_descr) (buf : bytes) : unit =
  let len = Bytes.length buf in
  (* Printf.printf "sending chunk of length %d" len; print_newline (); *)
  let n = Unix.send fd (Util.raw_bytes_of_int len) 0 4 [] in
  if n < 4 then
    failwith "send_chunk: message header failed to send all at once.";
  let n = Unix.send fd buf 0 len [] in
  if n < len then
    failwith (Printf.sprintf "send_chunk: message of length %d failed to send all at once." len)

let recv_or_close fd buf offs len flags =
  let n = recv fd buf offs len flags in
  if n = 0 then raise End_of_file;
  n


let receive_chunk (fd : file_descr) : bytes =
  let buf4 = Bytes.make 4 '\x00' in
  let n = recv_or_close fd buf4 0 4 [] in
  if n < 4 then
    failwith "receive_chunk: message header did not arrive all at once.";
  let len = Util.int_of_raw_bytes buf4 in
  let buf = Bytes.make len '\x00' in
  let n = recv_or_close fd buf 0 len [] in
  (* Printf.printf "received chunk of length %d" len; print_newline (); *)
  if n < len then
    failwith
        (Printf.sprintf "receive_chunk: message of length %d did not arrive all at once." len);
  buf

let get_cfg err_msg =
  match !the_cfg with
  | None -> failwith (err_msg ^ " called before the_cfg was set")
  | Some cfg -> cfg

let get_write_fd name =
  try Hashtbl.find write_fds name
  with Not_found ->
    let write_fd = socket PF_INET SOCK_STREAM 0 in
    let cfg = get_cfg "get_write_fd" in
    let (ip, port) = get_addr_port cfg name in
    let entry = gethostbyname ip in
    let node_addr = ADDR_INET (Array.get entry.h_addr_list 0, port) in
    let chunk = Bytes.of_string (string_of_nat cfg.me) in
    connect write_fd node_addr;
    send_chunk write_fd chunk;
    Hashtbl.add write_fds name write_fd;
    write_fd

let setup cfg =
  Printexc.record_backtrace true;
  the_cfg := Some cfg;
  if log then Printf.printf "initial state is: %a\n%!" print_state (Obj.magic cfg.st);
  let (_, port) = get_addr_port cfg cfg.me in
  (* Printf.printf "listening on port %d" port; print_newline (); *)
  setsockopt listen_fd SO_REUSEADDR true;
  bind listen_fd (ADDR_INET (inet_addr_any, port));
  listen listen_fd 8;
  Printf.printf "listening on port %d\n%!" port


let new_conn () =
  if log then print_endline "new connection!";
  let (node_fd, node_addr) = accept listen_fd in
  let chunk = receive_chunk node_fd in
  let node = Bytes.to_string chunk in
  let name = nat_of_string node in
  Hashtbl.add read_fds node_fd name;
  (* ignore (get_write_fd name); *)
  if log then begin
    Printf.printf "done processing new connection from node %s" node;
    print_newline ()
  end

(* A peer closed its connection (e.g. the client finished): forget it. *)
let drop_conn fd =
  if log then Printf.printf "connection from node %s closed\n%!"
                (string_of_nat (get_name_for_read_fd fd));
  let name = get_name_for_read_fd fd in
  Hashtbl.remove read_fds fd;
  Unix.close fd;
  (* Also forget our connection to it, so a later node with the same name
     (e.g. the next client run) gets a fresh connection. *)
  match Hashtbl.find_opt write_fds name with
  | Some wfd -> Hashtbl.remove write_fds name; Unix.close wfd
  | None -> ()

let get_all_read_fds () =
  Hashtbl.fold (fun fd _ acc -> fd :: acc) read_fds []

let serialize_msg l tag msg =
  Marshal.to_string (int_of_nat l, int_of_nat tag, List.map int_of_nat msg) []

let deserialize_msg s =
  let (l, tag, msg) = Marshal.from_string s 0 in
  (nat_of_int l, nat_of_int tag, List.map nat_of_int msg)

let recv_msg fd =
  let chunk = receive_chunk fd in
  let (l, tag, msg) = deserialize_msg (Bytes.to_string chunk) in
  let src = get_name_for_read_fd fd in
  if log then begin
    Printf.printf "got msg in protocol %a with tag = %a, contents = %a from %s" print_nat l print_nat tag (print_list print_nat) msg (string_of_nat src);
    print_newline ()
  end;
  if !record_latency && tag = 1 then
    latencies := (Unix.gettimeofday () -. !last_request_sent) :: !latencies;
  (l, src, tag, msg)

(* Blocking receive: wait in select (no timeout) until a peer has a message,
   accepting new connections and dropping closed ones along the way. This
   replaces the original non-blocking poll (select with timeout 0.0): it is
   equivalent to the program's receive loop seeing None some number of times
   and then Some, which is an execution the verified program allows. *)
let rec recv_blocking () =
  let fds = listen_fd :: get_all_read_fds () in
  Timed.enter_idle ();
  let (ready_fds, _, _) = select fds [] [] (-1.0) in
  Timed.leave_idle ();
  let t1 = Timed.now () in
  if List.mem listen_fd ready_fds then new_conn ();
  let rec first = function
    | [] -> None
    | fd :: fds when fd = listen_fd -> first fds
    | fd :: fds ->
       match recv_msg fd with
       | m -> Some m
       | exception End_of_file -> drop_conn fd; first fds in
  let r = first ready_fds in
  Timed.add_network (Timed.now () -. t1);
  match r with
  | Some m -> m
  | None -> recv_blocking ()

let send_msg l dst tag msg =
  if log then begin
    Printf.printf "sending msg in protocol %a with tag = %a, contents = %a to %s" print_nat l print_nat tag (print_list print_nat) msg (string_of_nat dst);
    print_newline ()
  end;
  let t0 = Timed.now () in
  if !record_latency && tag = 0 then last_request_sent := Unix.gettimeofday ();
  let fd = get_write_fd dst in
  let s = serialize_msg l tag msg in
  let chunk = Bytes.of_string s in
  send_chunk fd chunk;
  Timed.add_network (Timed.now () -. t0)

let get_current_state () =
  let cfg = get_cfg "get_current_sate" in
  (*Printf.printf "current state is: %a\n%!" print_state (Obj.magic cfg.st); *)
  cfg.st

let set_current_state x =
  let cfg = get_cfg "set_current_state" in
  (* Printf.printf "setting state to: %a\n%!" print_state (Obj.magic cfg.st); *)
  cfg.st <- x

let get_protocol_state l =
  let st = get_current_state () in
  State.StateGetters.getStatelet st l

let set_protocol_state l x' =
  let st = get_current_state () in
  set_current_state (Unionmap.UMDef.upd Ordtype.nat_ordType l x' st)

let update_my_state l this x' =
  (* Printf.printf "setting state to %a" (print_heap print_ordered_sort_which_is_nat (super_hacky_print_system_heap (Obj.magic this))) x';
  print_newline(); *)
  let dstatelet = get_protocol_state l in
  let dstatelet' =
    { State.dstate = Unionmap.UMDef.upd Ordtype.nat_ordType (Obj.magic this) x'
                                        dstatelet.State.dstate;
      State.dsoup = dstatelet.State.dsoup } in
  set_protocol_state l dstatelet'

