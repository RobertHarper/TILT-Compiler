(*$import Prelude Name TilWord32 Sequence Prim Nil NilSubst Alpha *)

signature NILUTIL =
  sig

    (* The free??? functions take 
         (1) a flag indicating whether to look inside kinds (true when not present)
         (2) an integer indicating the minimum lambda depth a free variable has to be 
         (3) the object whose free variables are needed
       The free variables are returned as set.  
       When 2 sets are returned, the first is the term level fvs followed by the type level ones 
    *)
    val freeExpConVarInExnHandler  : bool * int * Nil.exp  -> Name.VarSet.set * Name.VarSet.set
    val freeExpConVarInExp  : bool * int * Nil.exp  -> Name.VarSet.set * Name.VarSet.set
    val freeExpConVarInCon  : bool * int * Nil.con  -> Name.VarSet.set * Name.VarSet.set
    val freeExpConVarInKind : bool * int * Nil.kind -> Name.VarSet.set * Name.VarSet.set
    val freeExpConVarInBnd  : bool * int * Nil.bnd  -> Name.VarSet.set * Name.VarSet.set
    val freeConVarInCon     : bool * int * Nil.con  -> Name.VarSet.set
    val freeConVarInKind    :        int * Nil.kind -> Name.VarSet.set       
    val freeVarInKind       :        int * Nil.kind -> Name.VarSet.set

    val varBoundByCbnd : Nil.conbnd  -> Nil.var
    val varsBoundByCbnds : Nil.conbnd list -> Nil.var list
    val varsBoundByBnds : Nil.bnd list -> Nil.var list

    val muExpand : bool * (Nil.var,Nil.con) Nil.sequence * Nil.var -> Nil.con
    val generate_tuple_label : int -> Name.label
(*    val exp_tuple : Nil.exp list -> Nil.exp*)
    val con_tuple : Nil.con list -> Nil.con
    val con_tuple_inject : Nil.con list -> Nil.con
    val kind_tuple : Nil.kind list -> Nil.kind
    val kind_type_tuple : int -> Nil.kind
    val unit_con : Nil.con
    val unit_exp : Nil.exp
    val match_exn : Nil.exp
    val bool_con : Nil.con
    val string_con : Nil.con
    val exn_con : Nil.con
    val true_exp : Nil.exp
    val false_exp : Nil.exp
    val int_con : Nil.con   (* 32-bit ints *)
    val char_con : Nil.con  (* 8-bit ints *)
    val function_type : Nil.openness -> Nil.function -> Nil.con
    val convert_sum_to_special : Nil.con * TilWord32.word -> Nil.con
    val allprim_uses_carg : Nil.allprim -> bool

    val flattenCbnds : Nil.conbnd list -> Nil.conbnd list
    val makeLetC : Nil.conbnd list -> Nil.con -> Nil.con
    val makeLetE : Nil.letsort -> Nil.bnd list -> Nil.exp -> Nil.exp
    val makeAppE : Nil.exp -> Nil.con list -> Nil.exp list -> Nil.exp list -> Nil.exp
    val makeProjC : Nil.con -> Nil.label list -> Nil.con
    val makeExpB : Nil.var * Nil.niltrace * Nil.exp -> Nil.bnd list
    val extractCbnd : Nil.conbnd -> Nil.var * Nil.con

    val effect : Nil.exp -> bool (* could the expression have an effect?
                                    assumes expression is A-normal *)


    val same_openness : Nil.openness * Nil.openness -> bool
    val same_effect   : Nil.effect * Nil.effect -> bool
    val sub_effect    : bool * Nil.effect * Nil.effect -> bool

    datatype 'a changeopt = NOCHANGE | CHANGE_RECURSE of 'a | CHANGE_NORECURSE of 'a
    type bound = {isConstr : bool,     (* Are we in a constructor?  Initially true. *)
		  level : int,
		  boundcvars : Name.VarSet.set,
		  boundevars : Name.VarSet.set}
    type handlers = 
	{exphandler : bound * Nil.exp -> Nil.exp changeopt,
	 bndhandler : bound * Nil.bnd -> (Nil.bnd list) changeopt,
	 conhandler : bound * Nil.con -> Nil.con changeopt,
	 cbndhandler : bound * Nil.conbnd -> (Nil.conbnd list) changeopt,
	 kindhandler : bound * Nil.kind -> Nil.kind changeopt}

    val default_exphandler : bound * Nil.exp -> Nil.exp changeopt
    val default_bndhandler : bound * Nil.bnd -> (Nil.bnd list) changeopt
    val default_conhandler : bound * Nil.con -> Nil.con changeopt
    val default_cbndhandler : bound * Nil.conbnd -> (Nil.conbnd list) changeopt
    val default_kindhandler : bound * Nil.kind -> Nil.kind changeopt

    val exp_rewrite : handlers -> Nil.exp -> Nil.exp
    val bnd_rewrite : handlers -> Nil.bnd -> Nil.bnd list
    val kind_rewrite : handlers -> Nil.kind -> Nil.kind
    val con_rewrite : handlers -> Nil.con -> Nil.con
    val cbnd_rewrite : handlers -> Nil.conbnd -> Nil.conbnd

    val exp_size : Nil.exp -> int
    val con_size : Nil.con -> int
    val kind_size : Nil.kind -> int
    val module_size : Nil.module -> int

    val primequiv : Nil.primcon * Nil.primcon -> bool
    val covariant_prim: Nil.primcon -> bool
(*    type alpha_context

    val alpha_equiv_con' : (alpha_context*alpha_context) -> Nil.con * Nil.con -> bool

    val alpha_equiv_kind' : (alpha_context*alpha_context) -> Nil.kind * Nil.kind -> bool

    val alpha_sub_kind : Nil.kind * Nil.kind -> bool
*)

    val alpha_subequiv_con : bool -> Nil.con * Nil.con -> bool
    val alpha_equiv_con    : Nil.con * Nil.con -> bool
    val alpha_equiv_kind   : Nil.kind * Nil.kind -> bool

    val sub_phase : Nil.phase * Nil.phase -> bool

    val is_var_e : Nil.exp -> bool
    val is_mu_c : Nil.con -> bool
    val is_closed_value : Nil.exp -> bool

    val map_annotate : (Nil.con -> Nil.con) -> Nil.con -> Nil.con 

    val strip_var : Nil.con -> Nil.var option
    val strip_exntag : Nil.con -> Nil.con option
    val strip_recursive : Nil.con -> (bool * (Nil.var,Nil.con) Sequence.sequence) option
    val strip_boxfloat : Nil.con -> Prim.floatsize option
    val strip_float : Nil.con -> Prim.floatsize option
    val strip_int : Nil.con -> Prim.intsize option
    val strip_sum : Nil.con -> (Nil.w32 * Nil.w32 * Nil.w32 option * Nil.con) option
    val strip_arrow : Nil.con -> {openness : Nil.openness, effect : Nil.effect,
				  isDependent : bool,
				  tFormals : (Nil.var*Nil.kind) list,
				  eFormals : (Nil.var option * Nil.con) list,
				  fFormals : Nil.w32,
				  body_type : Nil.con} option
    val strip_externarrow : Nil.con -> (Nil.con list * Nil.con) option
    val strip_record : Nil.con -> (Nil.label list * Nil.var list option * Nil.con list) option
    val strip_crecord : Nil.con -> (Nil.label*Nil.con) list option
    val strip_proj : Nil.con -> (Nil.con*Nil.label) option
    val strip_prim : Nil.con -> (Nil.primcon*Nil.con list) option
    val strip_app : Nil.con -> (Nil.con*Nil.con list) option
    val strip_coercion : Nil.con -> {vars:Nil.var list,from:Nil.con,to:Nil.con} option
    val strip_annotate : Nil.con -> Nil.con

    (*strip_to_depth n c, n>=1
     * Strip off all annotations so that c can be pattern matched to depth n
     * Always goes at least one deep.
     *)
    val strip_to_depth : int -> Nil.con -> Nil.con

    (*Common depths*)
    val strip_one   : Nil.con -> Nil.con
    val strip_two   : Nil.con -> Nil.con
    val strip_three : Nil.con -> Nil.con  

    val alpha_mu : (Nil.var -> bool) -> (Nil.var * Nil.con) list -> (Nil.var * Nil.con) list
    val is_exn_con : Nil.con -> bool
    val is_var_c : Nil.con -> bool
    val is_float_c : Nil.con -> bool
    val is_unit_c : Nil.con -> bool

    val singletonize : (Nil.kind * Nil.con) -> Nil.kind
    val selfify  : (Nil.con * Nil.kind) -> Nil.kind
    val selfify' : (Nil.con * Nil.kind * NilSubst.con_subst) -> Nil.kind

    val get_arrow_return : Nil.con -> Nil.con option

    (*Get the kind of a projection*)
    val project_from_kind : ((Nil.label * Nil.var), Nil.kind) Nil.sequence * Nil.con * Nil.label -> Nil.kind
    val project_from_kind_nondep : Nil.kind * Nil.label -> Nil.kind

    val path2con : Nil.var * Nil.label list -> Nil.con

    val is_taglike : Nil.con -> bool

    val sum_project_carrier_type : Nil.con -> Nil.con

    (* If optional variable argument is present, then the record will be bound
     * to that variable, and the expression returned will simply be that var
     *)
    val mk_record_with_gctag : 
      (Nil.label list) * (Nil.niltrace list option) * 
      (Nil.con list) * (Nil.exp list) * Nil.var option->  (Nil.bnd list * Nil.exp) 

  end
