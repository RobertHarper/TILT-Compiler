(*$import IL NILCONTEXT *)
signature TONIL =
sig
    structure Il : IL
    structure Nil : NIL

    val use_imprecise_kind_at_bind : bool ref

    val debug : bool ref
    val full_debug : bool ref
    val do_kill_cpart_of_functor : bool ref
    val omit_datatype_bindings : bool ref
    val do_memoize : bool ref

    val phasesplit : Il.context *  (Il.sbnd option * Il.context_entry) list -> Nil.module

    val elaborator_specific_optimizations : bool ref
    val optimize_empty_structure : bool ref

end
