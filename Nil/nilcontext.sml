(* See nilcontextpre.sml for the main implementation. This file just
 * links in some extra parameters.
 *)

structure NilContext :> NILCONTEXT where type context = NilContextPre.context =
  struct
    open NilContextPre
    val insert_exp = insert_exp_pre Normalize.type_of
    val insert_bnd = insert_bnd_pre Normalize.type_of
  end
