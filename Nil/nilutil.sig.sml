signature NILUTIL =
  sig
    structure Nil : NIL

    val substConInExp : (Nil.var -> Nil.con option) -> Nil.exp -> Nil.exp
    val substConInCon : (Nil.var -> Nil.con option) -> Nil.con -> Nil.con
    val substConInKind : (Nil.var -> Nil.con option) -> Nil.kind -> Nil.kind
    val substExpInExp : (Nil.var -> Nil.exp option) -> Nil.exp -> Nil.exp

    val freeExpVarInExp : Nil.exp -> Nil.var list (* returns free term-level free variables *)

    val muExpand : (Nil.var,Nil.con) Nil.sequence * Nil.var -> Nil.con
    val generate_tuple_label : int -> Name.label
    val exp_tuple : (Nil.exp * Nil.con) list -> Nil.exp
    val con_tuple : Nil.con list -> Nil.con
    val con_tuple_inject : Nil.con list -> Nil.con
    val kind_tuple : Nil.kind list -> Nil.kind
    val unit_con : Nil.con
    val unit_exp : Nil.exp
    val match_exn : Nil.exp
    val bool_con : Nil.con
    val string_con : Nil.con
    val true_exp : Nil.exp
    val false_exp : Nil.exp

    val con_free_convar : Nil.con -> Name.var list
    val convar_occurs_free : Name.var * Nil.con -> bool

    val same_openness : Nil.openness * Nil.openness -> bool
    val same_effect : Nil.effect * Nil.effect -> bool

    datatype 'a changeopt = NOCHANGE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a
    type bound = {boundcvars : Nil.kind Name.VarMap.map,
		  boundevars : Nil.con Name.VarMap.map}
    type handlers = ((bound * Nil.exp -> Nil.exp changeopt) *
		     (bound * Nil.bnd -> (Nil.bnd list) changeopt) *
		     (bound * Nil.con -> Nil.con changeopt) *
		     (bound * Nil.conbnd -> (Nil.conbnd list) changeopt) *
		     (bound * Nil.kind -> Nil.kind changeopt))
    val exp_rewrite : handlers -> Nil.exp -> Nil.exp
    val bnd_rewrite : handlers -> Nil.bnd -> Nil.bnd list
    val kind_rewrite : handlers -> Nil.kind -> Nil.kind
    val con_rewrite : handlers -> Nil.con -> Nil.con

    val primequiv : Nil.primcon * Nil.primcon -> bool
    type alpha_context

    val alpha_equiv_con' : (alpha_context*alpha_context) -> Nil.con * Nil.con -> bool
    val alpha_equiv_con : Nil.con * Nil.con -> bool

    val alpha_equiv_kind' : (alpha_context*alpha_context) -> Nil.kind * Nil.kind -> bool
    val alpha_equiv_kind : Nil.kind * Nil.kind -> bool

    val alpha_sub_kind' : (alpha_context*alpha_context) -> Nil.kind * Nil.kind -> bool
    val alpha_sub_kind : Nil.kind * Nil.kind -> bool

    val alpha_normalize_con : Nil.con -> Nil.con
    val alpha_normalize_con' : alpha_context -> Nil.con -> Nil.con

    val alpha_normalize_kind : Nil.kind -> Nil.kind
    val alpha_normalize_kind' : alpha_context -> Nil.kind -> Nil.kind

    val sub_phase : Nil.phase * Nil.phase -> bool
    val get_phase : Nil.kind -> Nil.phase
  end
