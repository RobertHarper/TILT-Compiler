(*$import Nil NilContext *)

(*
 Verification that a module contains no unbound variables
*)

signature BOUNDCHECK = 
  sig
    val check_mod : NilContext.context * Nil.module -> bool
    (* check_mod (ctxt, module) ==> true iff no unbound variables occur in module with starting context ctxt *)
  end
