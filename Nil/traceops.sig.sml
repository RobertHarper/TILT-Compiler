(*$import TraceInfo Nil NilContext Name *)

signature TRACEOPS = 
sig

  (* Get a trace if possible, optimizing for the least computation
   * needed.
   *)
  val get_trace : NilContext.context * Nil.con -> TraceInfo.traceinfo option

  (*  Just get a trace from the con as syntactically given.
   *)
  val get_trace' : Nil.con -> TraceInfo.traceinfo option

  (* Best effort: uses get_trace' and returns TraceUnknown if 
   * it can't figure it out. 
   *)
  val con2trace : Nil.con -> Nil.niltrace

  val get_free_vars : Nil.niltrace -> Name.VarSet.set
  val get_free_vars' : TraceInfo.traceinfo -> Name.VarSet.set
  val valid_trace : NilContext.context * Nil.niltrace -> bool
end
