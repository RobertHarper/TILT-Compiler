(*$import NIL *)
signature NILUTIL =
  sig
    structure Nil : NIL

    (* if flag is true, will look inside kinds *)
    val freeExpConVarInExp : bool * Nil.exp -> Nil.var list * Nil.var list (* free term and type level vars *)
    val freeConVarInCon    : bool * Nil.con -> Nil.var list (* free type level vars *)
    val freeConVarInKind   : Nil.kind -> Nil.var list (* free type level vars *)

    val is_shape : Nil.kind -> bool
    val muExpand : bool * (Nil.var,Nil.con) Nil.sequence * Nil.var -> Nil.con
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
    val int_con : Nil.con   (* 32-bit ints *)
    val char_con : Nil.con  (* 8-bit ints *)

    val letc : Nil.conbnd list * Nil.con -> Nil.con
    val lete : Nil.bnd list * Nil.exp -> Nil.exp


    val effect : Nil.exp -> bool (* could the expression have an effect *)

    val con_free_convar : Nil.con -> Name.var list
    val convar_occurs_free : Name.var * Nil.con -> bool
    val expvars_occur_free : Name.var list * Nil.exp -> bool

    val same_openness : Nil.openness * Nil.openness -> bool
    val same_effect : Nil.effect * Nil.effect -> bool

    datatype 'a changeopt = NOCHANGE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a
    type bound = {boundcvars : Name.VarSet.set,
		  boundevars : Name.VarSet.set}
    type handlers = ((bound * Nil.exp -> Nil.exp changeopt) *
		     (bound * Nil.bnd -> (Nil.bnd list) changeopt) *
		     (bound * Nil.con -> Nil.con changeopt) *
		     (bound * Nil.conbnd -> (Nil.conbnd list) changeopt) *
		     (bound * Nil.kind -> Nil.kind changeopt))
    val exp_rewrite : handlers -> Nil.exp -> Nil.exp
    val bnd_rewrite : handlers -> Nil.bnd -> Nil.bnd list
    val kind_rewrite : handlers -> Nil.kind -> Nil.kind
    val con_rewrite : handlers -> Nil.con -> Nil.con

    val exp_size : Nil.exp -> int
    val con_size : Nil.con -> int
    val kind_size : Nil.kind -> int
    val module_size : Nil.module -> int

    val primequiv : Nil.primcon * Nil.primcon -> bool
    type alpha_context

    val alpha_equiv_con' : (alpha_context*alpha_context) -> Nil.con * Nil.con -> bool
    val alpha_equiv_con : Nil.con * Nil.con -> bool

    val alpha_equiv_kind' : (alpha_context*alpha_context) -> Nil.kind * Nil.kind -> bool
    val alpha_equiv_kind : Nil.kind * Nil.kind -> bool

    val alpha_sub_kind : Nil.kind * Nil.kind -> bool

    val alpha_normalize_exp : Nil.exp -> Nil.exp
    val alpha_normalize_exp' : (alpha_context * alpha_context) -> Nil.exp -> Nil.exp

    val alpha_normalize_con : Nil.con -> Nil.con
    val alpha_normalize_con' : alpha_context -> Nil.con -> Nil.con

    val alpha_normalize_kind : Nil.kind -> Nil.kind
    val alpha_normalize_kind' : alpha_context -> Nil.kind -> Nil.kind

    val sub_phase : Nil.phase * Nil.phase -> bool

    val is_var_e : Nil.exp -> bool

    val map_annotate : (Nil.con -> Nil.con) -> Nil.con -> Nil.con 

    val strip_var : Nil.con -> Nil.var option
    val strip_exntag : Nil.con -> Nil.con option
    val strip_recursive : Nil.con -> (bool * (Nil.var,Nil.con) Sequence.sequence) option
    val strip_boxfloat : Nil.con -> Nil.Prim.floatsize option
    val strip_float : Nil.con -> Nil.Prim.floatsize option
    val strip_int : Nil.con -> Nil.Prim.intsize option
    val strip_sum : Nil.con -> (Nil.w32 * Nil.w32 * Nil.w32 option * Nil.con) option
    val strip_arrow : Nil.con -> 
      (Nil.openness*Nil.effect*(Nil.var*Nil.kind) list*Nil.con list*Nil.w32*Nil.con) option
    val strip_record : Nil.con -> (Nil.label list * Nil.con list) option
    val strip_crecord : Nil.con -> (Nil.label*Nil.con) list option
    val strip_proj : Nil.con -> (Nil.con*Nil.label) option
    val strip_prim : Nil.con -> (Nil.primcon*Nil.con list) option
    val strip_app : Nil.con -> (Nil.con*Nil.con list) option

    val alpha_mu : (Nil.var -> bool) -> (Nil.var * Nil.con) list -> (Nil.var * Nil.con) list
    val is_exn_con : Nil.con -> bool
    val is_var_c : Nil.con -> bool
    val is_float_c : Nil.con -> bool
    val is_unit_c : Nil.con -> bool

    val singletonize : (Nil.kind * Nil.con) -> Nil.kind
    val selfify : (Nil.con * Nil.kind) -> Nil.kind

    val get_arrow_return : Nil.con -> Nil.con option
    val get_function_type : Nil.openness -> Nil.function -> Nil.con
    
  end
