(*$import TraceInfo Nil NilContext Util Normalize TRACEOPS *)

structure TraceOps :> TRACEOPS = 
struct

  structure TI = TraceInfo

  open Nil

  fun error s = Util.error "traceops.sml" s 

  fun get_trace_primcon (_, p, _) =
    (case p of 
       Int_c _ => SOME TI.Notrace_Int
     | Float_c _ => SOME TI.Notrace_Real
     | BoxFloat_c _ => SOME TI.Trace
     | Exn_c => SOME TI.Trace
     | Array_c => SOME TI.Trace
     | Vector_c => SOME TI.Trace
     | Ref_c => SOME TI.Trace
     | Exntag_c => SOME TI.Notrace_Int
     | Record_c _ => SOME TI.Trace
     | Sum_c _ => SOME TI.Trace
     | Vararg_c _ => SOME TI.Trace)

  fun get_trace  (ctxt, c) =
    let
       val (_, c') = Normalize.reduce_hnf (ctxt, c)
    in
       case c' of
          Prim_c (p, cs) =>
            get_trace_primcon (ctxt, p, cs)
        | AllArrow_c (Open,_,_,_,_,_,_) =>
            SOME TI.Trace
        | AllArrow_c (Closure,_,_,_,_,_,_) =>
            SOME TI.Trace
        | AllArrow_c (Code,_,_,_,_,_,_) =>
            SOME TI.Notrace_Code
        | ExternArrow_c _ => 
            SOME TI.Notrace_Code
        | Mu_c _ => 
            SOME TI.Trace
        | Var_c v => 
            SOME (TI.Compute (v, []))
        | Proj_c (Var_c v, l) => 
            SOME (TI.Compute (v, [l]))
        | Proj_c (Proj_c (Var_c v, l1), l2) => 
            SOME (TI.Compute (v, [l1,l2]))
        | Proj_c (Mu_c _, _) => 
            SOME TI.Trace
        | Proj_c _ => 
            NONE
        | App_c _ => 
            NONE
        | Typecase_c _ => 
            NONE
        | Annotate_c (_,c) => 
            get_trace (ctxt, c)
        | Let_c _ => 
            error "get_trace found Let_c after hnf"
        | Typeof_c _ => 
            error "get_trace found Typeof_c after hnf"
        | Crecord_c _ => 
            error "get_trace found Crecord_c"
        | Closure_c _ =>
            error "get_trace found Closure_c"
     end

  fun get_free_vars (TI.Compute (v,_)) = [v]
    | get_free_vars _ = []

end