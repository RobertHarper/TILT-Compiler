(*$import Nil NILCONTEXTPRE *)

(* See nilcontext.sig.sml for the signature of the main implementation.
 * This just describes a version parameters filled in.
 *)

signature NILCONTEXT = 
  sig
    include NILCONTEXTPRE
    val insert_exp : context * var *exp -> context
  end