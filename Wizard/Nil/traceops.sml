(*$import TraceInfo Nil NilContext Util Normalize TRACEOPS *)

structure TraceOps :> TRACEOPS = 
struct


  structure TI = TraceInfo

  open Nil Curtain

  fun error s = Util.error "traceops.sml" s 

  fun get_trace_primcon (_, p, _) =
    (case p of 
       Int_c _ => SOME TI.Notrace_Int
     | Float_c _ => SOME TI.Notrace_Real
     | BoxFloat_c _ => SOME TI.Trace
     | Exn_c => SOME TI.Trace
     | Array_c => SOME TI.Trace
     | Vector_c => SOME TI.Trace
     | Loc_c => error "what do I do with a locative?"
     | Exntag_c => SOME TI.Notrace_Int
     | Record_c _ => SOME TI.Trace
     | Sum_c _ => SOME TI.Trace
     | Vararg_c _ => SOME TI.Trace)

  fun get_trace  (ctxt, c) =
    let
       val (_, c') = Normalize.reduce_hnf (ctxt, c)
    in
       case C.expose c' of
          Prim_c (p, cs) =>
            get_trace_primcon (ctxt, p, cs)
        | AllArrow_c {openness=Open,...} => SOME TI.Trace
        | AllArrow_c {openness=Closure,...} => SOME TI.Trace
        | AllArrow_c {openness=Code,...} => SOME TI.Notrace_Code
        | ExternArrow_c _ => 
            SOME TI.Notrace_Code
        | Mu_c _ => 
            SOME TI.Trace
        | Var_c v => 
            SOME (TI.Compute (v, []))
        | Proj_c (c,l) =>
	    (case C.expose c
	       of (Var_c v) => 
		 SOME (TI.Compute (v, [l]))
		| Proj_c (c, l') => (case C.expose c
				       of Var_c v => SOME (TI.Compute (v, [l',l]))
					| _ => NONE)
		| Mu_c _ => 
		 SOME TI.Trace
		| _ => 
		 NONE)
        | App_c _ => 
            NONE
        | Typecase_c _ => 
            NONE
        | Let_c _ => 
            error "get_trace found Let_c after hnf"
        | Typeof_c _ => 
            error "get_trace found Typeof_c after hnf"
        | Crecord_c _ => 
            error "get_trace found Crecord_c"
        | Closure_c _ =>
            error "get_trace found Closure_c"
     end

  fun get_free_vars' (TI.Compute (v,_)) = [v]
    | get_free_vars' _ = []

  fun get_free_vars (TraceKnown tinfo) = get_free_vars' tinfo
    | get_free_vars (TraceCompute v) = [v]
    | get_free_vars _ = []

  fun valid_trace (ctxt, TraceKnown (TI.Compute(v,ls))) = 
      (* XXX Unsound approximation ! *)
      (* but calling con_valid is too heavyweight; it
         would print an error message *)
	  ((NilContext.find_con (ctxt, v); true)
	   handle NilContext.Unbound => false)
    | valid_trace (ctxt, TraceKnown _) = true
    | valid_trace (ctxt, TraceCompute v) =
	  ((NilContext.find_con (ctxt, v); true)
	   handle NilContext.Unbound => false)
    | valid_trace (_, TraceUnknown) = false

end