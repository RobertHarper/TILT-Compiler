(*$import Nil NILCONTEXTPRE *)

(* See nilcontext.sig.sml for the signature of the main implementation.
 * This just describes a version with parameters filled in.
 *)

signature NILCONTEXT = 
  sig
    include NILCONTEXTPRE

    (* insert_exp (ctx,v,e) ==> ctx,v:t where ctx |- e : t *)
    val insert_exp : context * var *exp -> context

    val insert_bnd  : context * Nil.bnd -> context

  end