signature TONIL =
sig

    val diag : bool ref
    val debug : bool ref
    val full_debug : bool ref
    val do_memoize : bool ref
    val killDeadImport : bool ref

    val phasesplit : Il.module -> Nil.module

    val elaborator_specific_optimizations : bool ref


end
