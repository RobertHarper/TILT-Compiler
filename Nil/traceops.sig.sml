(*$import Prelude TraceInfo Nil NilContext Name *)

signature TRACEOPS = 
sig

  val get_trace : NilContext.context * Nil.con -> TraceInfo.traceinfo option

  val get_free_vars : Nil.niltrace -> Name.VarSet.set
  val get_free_vars' : TraceInfo.traceinfo -> Name.VarSet.set
  val valid_trace : NilContext.context * Nil.niltrace -> bool
end