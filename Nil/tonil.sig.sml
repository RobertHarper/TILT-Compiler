(*$import Prelude Il Nil *)
signature TONIL =
sig

    val debug : bool ref
    val full_debug : bool ref
    val do_memoize : bool ref
    val killDeadImport : bool ref

    val phasesplit : Il.context *  (Il.sbnd option * Il.context_entry) list -> Nil.module

    val elaborator_specific_optimizations : bool ref


end
