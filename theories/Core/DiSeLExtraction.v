From DiSeL Require Import While.
From Coq Require Import ExtrOcamlBasic.
(* Extract nat as OCaml int. Note: mathcomp's ssrnat.eqn is still extracted as a
   recursive countdown (O(min m n) per comparison), and Nat.sub uses the
   polymorphic Stdlib.max (generic comparison). Both are left as-is for now. *)
From Coq Require Import ExtrOcamlNatInt.

Extraction Inline ssrbool.SimplPred.

Extract Constant Actions.Actions.tryrecv_action_wrapper => "DiSeL.tryrecv_action_wrapper".
Extract Constant Actions.Actions.send_action_wrapper => "DiSeL.send_action_wrapper".
Extract Constant Actions.Actions.skip_action_wrapper => "DiSeL.skip_action_wrapper".

Extract Constant HoareTriples.act_prog => "DiSeL.act_prog".
Extract Constant HoareTriples.ret_prog => "DiSeL.ret_prog".
Extract Constant HoareTriples.bnd_prog => "DiSeL.bnd_prog".
Extract Constant HoareTriples.DTLattice.sup_prog => "DiSeL.sup_prog".
Extract Constant HoareTriples.inject_prog => "DiSeL.inject_prog".
Extract Constant HoareTriples.with_inv_prog => "DiSeL.with_inv_prog".

Extract Constant HoareTriples.ffix => "DiSeL.ffix".
Extract Constant While.while => "DiSeL.coq_while".

Extract Inductive HoareTriples.prog => "DiSeL.prog" ["DiSeL.mkProg"] "DiSeL.elimProg".

Extract Inductive Actions.Actions.action => "DiSeL.action" ["DiSeL.mkAction"] "DiSeL.elimAction".
