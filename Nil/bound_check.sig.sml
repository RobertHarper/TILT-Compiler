(*$import Prelude Nil NilContext *)
signature BOUNDCHECK = 
  sig
    val check_mod : NilContext.context * Nil.module -> bool
  end