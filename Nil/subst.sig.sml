signature NILSUBST = 
  sig
    type exp
    type con
    type kind
    type bnd

    type var = Name.var

    type 'a subst

    val debug : bool ref
    val stats : bool ref
    val get_stats : unit -> {substituted:int,
			     short_circuited:int,
			     average_size:int,
			     average_sc_size:int}

    val reset_stats : unit -> unit

    val empty : unit -> 'a subst
	
    val fromList : (var * 'a) list -> 'a subst 

    val add : 'a subst -> (var * 'a) -> 'a subst 

    val substitute : 'a subst -> var -> 'a option

    val is_empty : 'a subst -> bool

    (* val compose : ('a subst -> 'a -> 'a) -> ('a subst * 'a subst) -> 'a subst 
     *  subst_fn (compose subst_fn (subst2,subst1))
     *  is equivalent to (subst_fn subst2) o (subst_fn subst1)
     *)
    val compose : ('a subst -> 'a -> 'a) -> ('a subst * 'a subst) -> 'a subst 

    val substConInCon : con subst -> con -> con
    val substConInKind : con subst -> kind -> kind
    val substConInExp : con subst -> exp -> exp
    val substConInBnd : con subst -> bnd -> bnd * con subst
    val substExpInExp : exp subst -> exp -> exp
    val substExpConInExp : (exp subst * con subst) -> exp -> exp

    val con_subst_compose : (con subst * con subst) -> con subst
    val varConKindSubst : var -> con -> kind -> kind
    val varConConSubst : var -> con -> con -> con

  end
