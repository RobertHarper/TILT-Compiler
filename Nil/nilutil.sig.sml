signature NILUTIL =
  sig
    structure Nil : NIL

    val substConInCon : (Nil.var -> Nil.con option) -> Nil.con -> Nil.con
    val substConInKind : (Nil.var -> Nil.con option) -> Nil.kind -> Nil.kind

    val con_free_convar : Nil.con -> Name.var list
    val convar_occurs_free : Name.var * Nil.con -> bool
    val alpha_equiv_con : Nil.con * Nil.con -> bool
    val alpha_equiv_kind : Nil.kind * Nil.kind -> bool
    val alpha_sub_kind : Nil.kind * Nil.kind -> bool

    val same_openness : Nil.openness * Nil.openness -> bool
    val same_effect : Nil.effect * Nil.effect -> bool
  end
