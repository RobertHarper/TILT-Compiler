(*$import Nil NILCONTEXTPRE *)
signature NILCONTEXT = 
  sig
    include NILCONTEXTPRE
    val find_std_con : context * var -> con
  end