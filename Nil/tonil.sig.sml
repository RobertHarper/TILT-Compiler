signature TONIL =
sig
    structure Il : IL
    structure NilContext : NILCONTEXT

    val debug : bool ref
    val full_debug : bool ref
    val do_kill_cpart_of_functor : bool ref
    val omit_datatype_bindings : bool ref
    val do_memoize : bool ref

    val xcompunit : Il.context -> 
                    (Name.var * Name.var) Name.VarMap.map ->
                    Il.sbnds ->
                    {nil_initial_context : NilContext.context,
		     nil_final_context : NilContext.context,
		     cu_bnds : NilContext.Nil.bnd list,
		     vmap : (Name.var * Name.var) Name.VarMap.map}

    val phasesplit : Il.context *  (Il.sbnd option * Il.context_entry) list -> NilContext.Nil.module

    val elaborator_specific_optimizations : bool ref
    val optimize_empty_structure : bool ref

end
