(*$import TraceInfo Nil NilContext *)

signature TRACEOPS = 
sig

  val get_trace : NilContext.context * Nil.con -> TraceInfo.traceinfo option

  val get_free_vars : TraceInfo.traceinfo -> Nil.var list

end