(* Runtime realization of Disel's program monad and network actions
   (see theories/Core/DiSeLExtraction.v). Nat-typed values are written as
   State.nid (= nat), which is int or Datatypes.nat depending on whether
   ExtrOcamlNatInt is used. Programs run in direct style:
   a 'a prog is just its result. *)

type 'a prog = 'a
val mkProg : 'a prog
type 'a action = 'a
val mkAction : 'a action

val ret_prog : 'n -> 'w -> 'a -> 'a prog
val act_prog : 'n -> 'w -> 'a action -> 'a prog
val bnd_prog : 'n -> 'w -> 'a prog -> ('a -> 'b prog) -> 'b prog
val sup_prog : 'n -> 'w -> 's -> 'a prog
val inject_prog : 'n -> 'v -> 'w -> 'k -> 'i -> 'a prog -> 'a prog
val with_inv_prog : 'p -> 'i -> 'n -> 'a prog -> 'a prog
val ffix : 'n -> 'w -> 's -> (('a -> 'b prog) -> 'a -> 'b prog) -> 'a -> 'b prog
val coq_while : 'n -> 'w -> ('a -> bool) -> ('a -> 'a prog) -> 'a -> 'a prog

val tryrecv_action_wrapper :
  Protocols.Protocols.protocol Unionmap.UMDef.tp * 'h ->
  State.nid -> 'p -> ((State.nid * State.nid) * State.nid list) option action
val send_action_wrapper :
  Protocols.Protocols.protocol Unionmap.UMDef.tp * 'h ->
  'p -> State.nid -> Ordtype.Ordered.sort ->
  Protocols.Transitions.send_trans -> State.nid list -> State.nid -> State.nid list action
val skip_action_wrapper :
  'c * 'h -> 'n -> 'l -> 'p -> (State.StateGetters.state -> 'x -> 'a) -> 'a action
