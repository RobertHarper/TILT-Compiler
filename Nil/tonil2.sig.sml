signature TONIL =
sig

    val diag : bool ref
    val debug : bool ref
    val chatlev : int ref

    val full_debug : bool ref
    val killDeadImport : bool ref

    val phasesplit : Il.module -> Nil.module
    val phasesplit_interface : Il.sc_module -> Nil.interface
    val elaborator_specific_optimizations : bool ref


end
