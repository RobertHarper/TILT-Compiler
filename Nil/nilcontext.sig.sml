(*$import Nil NILCONTEXTPRE *)
signature NILCONTEXT = 
  sig
    include NILCONTEXTPRE
    val find_std_con : context * var -> con
    val insert_exp : context * var *exp -> context
  end