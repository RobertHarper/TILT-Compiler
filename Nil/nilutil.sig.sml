signature NILUTIL =
  sig
    structure Nil : NIL

    val substConInCon : (Nil.var -> Nil.con option) -> Nil.con -> Nil.con
    val substConInKind : (Nil.var -> Nil.con option) -> Nil.kind -> Nil.kind

    val muExpand : (Nil.var,Nil.con) Nil.sequence * Nil.var -> Nil.con
    val freeVarsExp : Nil.exp -> Nil.var list (* returns free term-level free variables *)
    val generate_tuple_label : int -> Name.label
    val exp_tuple : Nil.exp list -> Nil.exp
    val con_tuple : Nil.con list -> Nil.con
    val con_tuple_inject : Nil.con list -> Nil.con
    val kind_tuple : Nil.kind list -> Nil.kind
    val unitcon : Nil.con
    val unitexp : Nil.exp
    val nomatch_exn : Nil.exp
    val boolcon : Nil.con

    val con_free_convar : Nil.con -> Name.var list
    val convar_occurs_free : Name.var * Nil.con -> bool
    val alpha_equiv_con : Nil.con * Nil.con -> bool
    val alpha_equiv_kind : Nil.kind * Nil.kind -> bool
    val alpha_sub_kind : Nil.kind * Nil.kind -> bool

    val same_openness : Nil.openness * Nil.openness -> bool
    val same_effect : Nil.effect * Nil.effect -> bool
  end
