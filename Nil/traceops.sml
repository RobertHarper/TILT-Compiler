(*$import Prelude TraceInfo Nil NilContext Util Normalize TRACEOPS Name List *)

structure TraceOps :> TRACEOPS = 
struct

  structure TI = TraceInfo

  open Nil

  val minimize_computes = Stats.tt "traceops_minimize_computes"

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
     | Vararg_c _ => SOME TI.Trace
     | GCTag_c => SOME TI.Notrace_Int)

  local
    type rank = TI.traceinfo option * int

    (* The path rank is the number of labels needed to compute it.
     * We want to choose the smallest ranked of the intermediates.
     * Among equally ranked paths, we choose the outermost.  
     *)
    fun path_rank c : rank =
      let
	fun loop (c,lbls,rank) = 
	  (case c
	     of Proj_c (c,l) => loop(c,l::lbls, rank + 1)
	      | Var_c v      => (SOME (TI.Compute(v,lbls)),rank)
	      | _            => (NONE,~1))
      in loop (c,[],0)
      end
    
    fun rank_min (fst as (tr1,r1),snd as (tr2,r2)) : rank = 
      (case tr1
	 of NONE => snd                         (* fst isn't a path *)
	  | _ => (case tr2 
		    of NONE => fst              (* snd isn't a path, fst is *)
		     | _ => if r2 < r1 then snd  (* snd is smaller, so keep it *)
			    else fst))           (* fst is smaller or equal, so keep it. *)
	 
    fun best_path []        = (NONE,~1)
      | best_path (c::rest) = rank_min(path_rank c,best_path rest) 
  in  
    val path2TraceCompute = #1 o path_rank
    val bestTraceCompute = #1 o best_path 
  end

  fun get_trace  (ctxt, c) =
    let
      (* If c is a path, then it will be the last element of paths 
       *)
       val (_, c',paths) = Normalize.reduce_hnf_list (ctxt, c)

    in
       case c' of
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
        | Proj_c (Mu_c _, _) => 
            SOME TI.Trace

	| Proj_c _ => 
	    if !minimize_computes then 
	      bestTraceCompute (c'::paths)  (*c' is a candidate, so include it*)
	    else 
	      path2TraceCompute c'
        | App_c _ => 
	    if !minimize_computes then 
	      bestTraceCompute paths        (*c' is not a candidate *)
	    else 
	      NONE 
	| Coercion_c _ => SOME TI.Notrace_Int
        | Typecase_c _ => NONE
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

  fun get_free_vars' (TI.Compute (v,_)) = Name.VarSet.singleton v
    | get_free_vars' _ = Name.VarSet.empty

  fun get_free_vars (TraceKnown tinfo) = get_free_vars' tinfo
    | get_free_vars (TraceCompute v) = Name.VarSet.singleton v
    | get_free_vars _ = Name.VarSet.empty

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